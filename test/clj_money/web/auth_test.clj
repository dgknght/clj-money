(ns clj-money.web.auth-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clj-http.core :as http]
            [cheshire.core :as json]
            [buddy.sign.jwt :as jwt]
            [ring.mock.request :as req]
            [dgknght.app-lib.core :as app-lib]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]
            [clj-money.models.users :as usrs])
  (:import java.io.ByteArrayInputStream))

(use-fixtures :each reset-db)

(deftest start-the-oauth-process
  (with-redefs [app-lib/uuid (constantly "abc123")]
    (is (http-redirect-to? "https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=google-id&redirect_uri=https%3A//www.mymoney.com/auth/google/callback&state=abc123&scope=email+profile"
                         (app (req/request :get "/auth/google/start"))))))

(defn- json-body
  [payload]
  (ByteArrayInputStream.
    (.getBytes
      (json/generate-string payload))))

(defn- json-response
  [payload]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json-body payload)})

(defn- json-response-fn
  [payload]
  (-> payload json-response constantly))

(def ^:private mocks
  {"https://www.googleapis.com/oauth2/v4/token"
   (json-response-fn {:access_token "hij789"})

   "https://www.googleapis.com/oauth2/v1/userinfo"
   (json-response-fn {:id "def456"
                      :email "john@doe.com"
                      :given_name "John"
                      :family_name "Doe"})})

(deftest handle-a-successful-oauth-callback-for-a-new-user
  (with-web-mocks [calls] mocks
    (with-redefs [jwt/sign (constantly "abc123")]
      (let [req-url "https://www.mymoney.com/auth/google/callback?state=abc123&code=4%2F0AVG7fiSLQL3IaL7KvlRvL1aOzG_q8MzSVzgPbT0hIK4vd56n0LqwDKUfdiER1SAUlV2wDg&scope=email+profile+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile+openid+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&authuser=0&prompt=none"
            res 
            (app (req/request :get req-url))]
        (is (http-redirect-to? "https://www.mymoney.com/" res)
            "The site redirects back to the root page")
        (is (http-response-with-cookie? "auth-token" "abc123" res)
            "The redirect contains the auth token")
        (is (comparable? #:user{:email "john@doe.com"
                                :first-name "John"
                                :last-name "Doe"}
                         (usrs/find-by-email "john@doe.com")))))))
