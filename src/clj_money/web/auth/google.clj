(ns clj-money.web.auth.google
  (:require [ring.util.response :as res]
            [clj-http.client :as http]
            [clj-money.web.system :as system]
            [clj-money.web.auth :refer [make-token make-json-request]]
            [clj-money.entities.identities :as idents]))

(defn- request-user-info
  [access-token]
  (make-json-request http/get
                     "https://www.googleapis.com/oauth2/v1/userinfo"
                     {:headers {"Authorization" (str "Bearer " access-token)}}))

(defn redirect-handler
  [{:oauth2/keys [access-tokens] :as request}]
  (if-let [token (get-in access-tokens [:google :token])]
    (let [secret     (-> request (system/component :services) :auth-secret)
          raw-info   (request-user-info token)
          user       (idents/find-or-create-from-profile [:google raw-info])
          auth-token (make-token user secret)]
      (-> (res/redirect "/")
          (res/set-cookie :auth-token
                          auth-token
                          {:path "/"})
          (res/set-cookie :profile-photo
                          (:picture raw-info)
                          {:path "/"})))
    (res/redirect "/?error=oauth_failed")))

(defn oauth2-profile
  [{:keys [client-id client-secret]}]
  (when client-id
    {:google
     {:authorize-uri    "https://accounts.google.com/o/oauth2/v2/auth"
      :access-token-uri "https://www.googleapis.com/oauth2/v4/token"
      :client-id        client-id
      :client-secret    client-secret
      :scopes           ["email" "profile"]
      :launch-uri       "/auth/google/start"
      :redirect-uri     "/auth/google/callback"
      :landing-uri      "/auth/google/done"}}))
