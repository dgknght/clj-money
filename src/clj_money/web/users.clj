(ns clj-money.web.users
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.config :refer [env]]
            [buddy.sign.jwt :as jwt]
            [clj-money.entities :as entities]))

(defn- extract-header-auth-token
  [{:keys [headers]}]
  (when-let [header-value (get-in headers ["authorization"])]
    (re-find #"(?<=Bearer ).*" header-value)))

(defn- extract-cookie-auth-token
  [{:keys [cookies]}]
  (get-in cookies ["auth-token" :value]))

(defn- extract-auth-token
  [req]
  (some #(% req) [extract-header-auth-token
                  extract-cookie-auth-token]))

(defn find-user-by-auth-token
  [req]
  (some-> req
          extract-auth-token
          (jwt/unsign (env :secret))
          :user-id
          entities/find))
