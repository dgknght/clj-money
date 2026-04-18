(ns clj-money.web.auth
  (:require [clojure.string :as str]
            [ring.util.response :as res]
            [buddy.sign.jwt :as jwt]
            [clj-http.client :as http]
            [clj-money.config :refer [env]]
            [jsonista.core :as json]
            [clj-money.entities.identities :as idents]))

(defn make-token
  [user]
  (jwt/sign {:user-id (:id user)} (env :secret)))

(defn- make-json-request
  [method uri options]
  (let [req (merge options {:accept :json})
        res (method uri req)]
    (json/read-value (:body res)
                     (json/object-mapper {:decode-key-fn true}))))

(defn- request-user-info
  [access-token]
  (make-json-request http/get
                     "https://www.googleapis.com/oauth2/v1/userinfo"
                     {:headers {"Authorization" (str "Bearer " access-token)}}))

(defn- request-github-user-info
  [access-token]
  (make-json-request http/get
                     "https://api.github.com/user"
                     {:headers {"Authorization" (str "Bearer " access-token)
                                "Accept" "application/vnd.github+json"}}))

(defn- normalize-github-profile
  [{:keys [id name email login]}]
  (let [[given-name family-name] (if (seq name)
                                   (str/split name #"\s+" 2)
                                   [login nil])]
    {:id (str id)
     :email email
     :given_name given-name
     :family_name family-name}))

(defn google-redirect-handler
  [request]
  (if-let [token (get-in request [:oauth2/access-tokens :google :token])]
    (let [user-info  (request-user-info token)
          user       (idents/find-or-create-from-profile [:google user-info])
          auth-token (make-token user)]
      (-> "/"
          res/redirect
          (res/set-cookie :auth-token auth-token {:path "/"})))
    (res/redirect "/?error=oauth_failed")))

(defn github-redirect-handler
  [request]
  (if-let [token (get-in request [:oauth2/access-tokens :github :token])]
    (let [user-info  (-> token
                         request-github-user-info
                         normalize-github-profile)
          user       (idents/find-or-create-from-profile [:github user-info])
          auth-token (make-token user)]
      (-> "/"
          res/redirect
          (res/set-cookie :auth-token auth-token {:path "/"})))
    (res/redirect "/?error=oauth_failed")))

(defn oauth2-profiles
  []
  (merge
    (when (env :google-client-id)
      {:google
       {:authorize-uri    "https://accounts.google.com/o/oauth2/v2/auth"
        :access-token-uri "https://www.googleapis.com/oauth2/v4/token"
        :client-id        (env :google-client-id)
        :client-secret    (env :google-client-secret)
        :scopes           ["email" "profile"]
        :launch-uri       "/auth/google/start"
        :redirect-uri     "/auth/google/callback"
        :landing-uri      "/auth/google/done"}})
    (when (env :github-client-id)
      {:github
       {:authorize-uri    "https://github.com/login/oauth/authorize"
        :access-token-uri "https://github.com/login/oauth/access_token"
        :client-id        (env :github-client-id)
        :client-secret    (env :github-client-secret)
        :scopes           ["user:email"]
        :launch-uri       "/auth/github/start"
        :redirect-uri     "/auth/github/callback"
        :landing-uri      "/auth/github/done"}})))
