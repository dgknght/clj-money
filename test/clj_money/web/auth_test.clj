(ns clj-money.web.auth-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [clj-http.core :as http]
            [jsonista.core :as json]
            [buddy.sign.jwt :as jwt]
            [ring.mock.request :as req]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]
            [clj-money.entities.users :as usrs])
  (:import java.io.ByteArrayInputStream
           java.net.URLDecoder))

(use-fixtures :each reset-db)

(deftest start-the-google-oauth-process
  (is (http-redirect-to?
        #"https://accounts\.google\.com/o/oauth2/v2/auth\?.*client_id=google-id.*scope=email"
        (app (req/request :get "/auth/google/start")))))

(deftest start-the-github-oauth-process
  (is (http-redirect-to?
        #"https://github\.com/login/oauth/authorize\?.*client_id=github-id"
        (app (req/request :get "/auth/github/start")))))

(defn- json-body
  [payload]
  (ByteArrayInputStream.
    (.getBytes
      (json/write-value-as-string payload))))

(defn- json-response
  [payload]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json-body payload)})

(defn- json-response-fn
  [payload]
  (-> payload json-response constantly))

(def ^:private google-mocks
  {"https://www.googleapis.com/oauth2/v4/token"
   (json-response-fn {:access_token "hij789"})

   "https://www.googleapis.com/oauth2/v1/userinfo"
   (json-response-fn {:id "def456"
                      :email "john@doe.com"
                      :given_name "John"
                      :family_name "Doe"})})

(def ^:private mocks google-mocks)

(def ^:private github-mocks
  {"https://github.com/login/oauth/access_token"
   (json-response-fn {:access_token "ghp_abc123"})

   "https://api.github.com/user"
   (json-response-fn {:id 12345
                      :email "jane@doe.com"
                      :name "Jane Doe"
                      :login "janedoe"})})

(defn- query-param
  "Extracts a single query parameter value from a URL string."
  [url param]
  (when-let [pair (->> (str/split url #"[\?&]")
                       (filter #(str/starts-with? % (str param "=")))
                       first)]
    (URLDecoder/decode (subs pair (inc (count param))) "UTF-8")))

(defn- extract-session-cookie
  "Extracts the ring-session name=value from a response's Set-Cookie headers,
  suitable for use as a Cookie request header."
  [response]
  (some->> (get-in response [:headers "Set-Cookie"])
           flatten
           (filter #(str/starts-with? % "ring-session="))
           first
           (re-find #"ring-session=[^;]+")))

(deftest handle-a-successful-oauth-callback-for-a-new-user
  (with-web-mocks [_calls] mocks
    (with-redefs [jwt/sign (constantly "abc123")]
      ;; Step 1: Start the OAuth flow to capture the state and session cookie
      (let [start-res    (app (req/request :get "/auth/google/start"))
            location     (get-in start-res [:headers "Location"])
            state        (query-param location "state")
            session-hdr  (extract-session-cookie start-res)
            ;; Step 2: Simulate Google's callback with the matching state
            callback-req (-> (req/request :get "/auth/google/callback")
                             (req/header "Cookie" session-hdr)
                             (assoc :query-params {"code"  "auth-code"
                                                   "state" state}))
            callback-res (app callback-req)]
        (is (http-redirect-to? "/auth/google/done" callback-res)
            "ring-oauth2 redirects to the landing URI after token exchange")
        ;; Step 3: Follow the redirect to /done with the updated session
        (let [done-session-hdr (extract-session-cookie callback-res)
              done-req         (-> (req/request :get "/auth/google/done")
                                   (req/header "Cookie" done-session-hdr))
              done-res         (app done-req)]
          (is (http-redirect-to? "/" done-res)
              "The done handler redirects to the root page")
          (is (http-response-with-cookie? "auth-token" "abc123" done-res)
              "The redirect contains the auth token")
          (is (comparable? #:user{:email "john@doe.com"
                                  :first-name "John"
                                  :last-name "Doe"}
                           (usrs/find-by-email "john@doe.com"))))))))

(deftest handle-a-successful-github-oauth-callback-for-a-new-user
  (with-web-mocks [_calls] github-mocks
    (with-redefs [jwt/sign (constantly "ghtoken123")]
      (let [start-res    (app (req/request :get "/auth/github/start"))
            location     (get-in start-res [:headers "Location"])
            state        (query-param location "state")
            session-hdr  (extract-session-cookie start-res)
            callback-req (-> (req/request :get "/auth/github/callback")
                             (req/header "Cookie" session-hdr)
                             (assoc :query-params {"code"  "gh-auth-code"
                                                   "state" state}))
            callback-res (app callback-req)]
        (is (http-redirect-to? "/auth/github/done" callback-res)
            "ring-oauth2 redirects to the landing URI after token exchange")
        (let [done-session-hdr (extract-session-cookie callback-res)
              done-req         (-> (req/request :get "/auth/github/done")
                                   (req/header "Cookie" done-session-hdr))
              done-res         (app done-req)]
          (is (http-redirect-to? "/" done-res)
              "The done handler redirects to the root page")
          (is (http-response-with-cookie? "auth-token" "ghtoken123" done-res)
              "The redirect contains the auth token")
          (is (comparable? #:user{:email "jane@doe.com"
                                  :first-name "Jane"
                                  :last-name "Doe"}
                           (usrs/find-by-email "jane@doe.com"))))))))
