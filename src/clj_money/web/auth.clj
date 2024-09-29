(ns clj-money.web.auth
  (:require [clojure.pprint :refer [pprint]]
            [ring.util.response :as res]
            [buddy.sign.jwt :as jwt]
            [clj-http.client :as http]
            [config.core :refer [env]]
            [cheshire.core :as json]
            [dgknght.app-lib.core :refer [uuid]]
            [lambdaisland.uri :refer [uri map->query-string]]
            [clj-money.models.identities :as idents]))

(defn- callback-url []
  {:pre [(env :site-protocol)
         (env :site-host)]}
  (-> (uri "/auth/google/callback")
      (assoc :host (env :site-host)
             :scheme (env :site-protocol))
      str))

(defn- redirect-url
  [state]
  (assert (env :google-client-id) "The google client has not been configured correctly")
  (-> (uri "/o/oauth2/v2/auth")
      (assoc :scheme "https"
             :host "accounts.google.com"
             :query (map->query-string {:response_type "code"
                                        :client_id (env :google-client-id)
                                        :redirect_uri (callback-url)
                                        :state state
                                        :scope "email profile"}))
      str))

(defn make-token
  [user]
  (jwt/sign {:user-id (:id user)} (env :secret)))

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
  ; TODO: check the state to see if it matches
  (let [{access-token :access_token} (request-access-token code)
        user-info (request-user-info access-token)
        user (idents/find-or-create-from-profile :google user-info)
        auth-token (make-token user)]
    (-> "/"
        res/redirect
        (res/set-cookie :auth-token auth-token {:path "/"}))))

(defmethod ^:private callback :error
  [{{:keys [error]} :params}]
  (res/redirect (str "/?error=" error)))

(defn- start [_]
  (let [state (str (uuid))]
    (res/redirect (redirect-url state))))

(def routes
  ["google"
   ["/start" {:get {:handler start}}]
   ["/callback" {:get {:handler callback}}]])
