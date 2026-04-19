(ns clj-money.web.auth.github
  (:require [clojure.string :as str]
            [ring.util.response :as res]
            [clj-http.client :as http]
            [clj-money.config :refer [env]]
            [clj-money.web.auth :refer [make-token make-json-request]]
            [clj-money.entities.identities :as idents]))

(defn- github-get
  [access-token path]
  (make-json-request http/get
                     (str "https://api.github.com" path)
                     {:headers {"Authorization" (str "Bearer " access-token)
                                "Accept" "application/vnd.github+json"}}))

(defn- request-primary-email
  [access-token]
  (->> (github-get access-token "/user/emails")
       (filter :primary)
       first
       :email))

(defn- request-user-info
  [access-token]
  (let [profile (github-get access-token "/user")
        email   (or (:email profile)
                    (request-primary-email access-token))]
    (assoc profile :email email)))

(defn- normalize-profile
  [{:keys [id name email login]}]
  (let [[given-name family-name] (if (seq name)
                                   (str/split name #"\s+" 2)
                                   [login nil])]
    {:id (str id)
     :email email
     :given_name given-name
     :family_name family-name}))

(defn redirect-handler
  [request]
  (if-let [token (get-in request [:oauth2/access-tokens :github :token])]
    (let [user-info  (-> token
                         request-user-info
                         normalize-profile)
          user       (idents/find-or-create-from-profile [:github user-info])
          auth-token (make-token user)]
      (-> "/"
          res/redirect
          (res/set-cookie :auth-token auth-token {:path "/"})))
    (res/redirect "/?error=oauth_failed")))

(defn oauth2-profile
  []
  (when (env :github-client-id)
    {:github
     {:authorize-uri    "https://github.com/login/oauth/authorize"
      :access-token-uri "https://github.com/login/oauth/access_token"
      :client-id        (env :github-client-id)
      :client-secret    (env :github-client-secret)
      :scopes           ["user:email"]
      :launch-uri       "/auth/github/start"
      :redirect-uri     "/auth/github/callback"
      :landing-uri      "/auth/github/done"}}))
