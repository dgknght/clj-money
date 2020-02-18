(ns clj-money.web.auth
  (:require [compojure.core :refer [defroutes GET]]
            [ring.util.response :as res]
            [buddy.sign.jwt :as jwt]
            [clj-http.client :as http]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-money.url :refer [protocol
                                   host
                                   path
                                   query
                                   format-url]]
            [clj-money.models.identities :as idents])
  (:import [java.util UUID]))

(defn- callback-url []
  {:pre [(env :site-protocol)
         (env :site-host)]}
  (-> (protocol (env :site-protocol))
      (host (env :site-host))
      (path :auth :google :callback)
      format-url))

(defn- redirect-url []
  (assert (env :google-client-id) "The google client has not been configured correctly")
  (-> (protocol "https")
      (host "accounts.google.com")
      (path :o :oauth2 :v2 :auth)
      (query {:response_type "code"
              :client_id (env :google-client-id)
              :redirect_uri (callback-url)
              :scope "email profile"})
      format-url))

(defn make-token
  [user]
  (jwt/sign {:user-id (:id user)} (env :secret)))

(defn- random-state []
  (str (UUID/randomUUID)))

(defn- make-json-request
  [method uri options]
  (let [req (merge options {:accept :json})
        res (method uri req)]
    (json/parse-string (:body res) true)))

(defn- request-access-token
  [code]
  (make-json-request http/post
                     "https://www.googleapis.com/oauth2/v4/token"
                     {:form-params {:code code
                                    :client_id (env :google-client-id)
                                    :client_secret (env :google-client-secret)
                                    :redirect_uri (callback-url)
                                    :grant_type "authorization_code"}}))

(defn- request-user-info
  [access-token]
  (make-json-request http/get
                     "https://www.googleapis.com/oauth2/v1/userinfo"
                     {:headers {"Authorization" (str "Bearer " access-token)}}))

(defmulti ^:private callback
  (fn [{:keys [params]}]
    (->> (keys params)
         (map #{:code :error})
         (filter identity)
         first)))

(defmethod ^:private callback :code
  [{{:keys [code]} :params}]
  (let [{access-token :access_token} (request-access-token code)
        user-info (request-user-info access-token)
        user (idents/find-or-create-from-profile (env :db) :google user-info)
        auth-token (make-token user)]
    (-> "/"
        res/redirect
        (res/set-cookie :auth-token auth-token {:path "/"}))))

(defmethod ^:private callback :error
  [{{:keys [error]} :params}]
  (res/redirect (str "/?error=" error)))

(defroutes routes
  (GET "/auth/google/start" []  (res/redirect (redirect-url)))
  (GET "/auth/google/callback" req (callback req)))
