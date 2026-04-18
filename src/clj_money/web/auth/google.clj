(ns clj-money.web.auth.google
  (:require [ring.util.response :as res]
            [clj-http.client :as http]
            [clj-money.config :refer [env]]
            [clj-money.web.auth :refer [make-token make-json-request]]
            [clj-money.entities.identities :as idents]))

(defn- request-user-info
  [access-token]
  (make-json-request http/get
                     "https://www.googleapis.com/oauth2/v1/userinfo"
                     {:headers {"Authorization" (str "Bearer " access-token)}}))

(defn redirect-handler
  [request]
  (if-let [token (get-in request [:oauth2/access-tokens :google :token])]
    (let [user-info  (request-user-info token)
          user       (idents/find-or-create-from-profile [:google user-info])
          auth-token (make-token user)]
      (-> "/"
          res/redirect
          (res/set-cookie :auth-token auth-token {:path "/"})))
    (res/redirect "/?error=oauth_failed")))

(defn oauth2-profile
  []
  (when (env :google-client-id)
    {:google
     {:authorize-uri    "https://accounts.google.com/o/oauth2/v2/auth"
      :access-token-uri "https://www.googleapis.com/oauth2/v4/token"
      :client-id        (env :google-client-id)
      :client-secret    (env :google-client-secret)
      :scopes           ["email" "profile"]
      :launch-uri       "/auth/google/start"
      :redirect-uri     "/auth/google/callback"
      :landing-uri      "/auth/google/done"}}))
