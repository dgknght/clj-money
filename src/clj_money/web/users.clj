(ns clj-money.web.users
  (:require [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]
            [buddy.sign.jwt :as jwt]
            [clj-money.models :as models]))

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
  (when-let [token (extract-auth-token req)]
    (models/find (:user-id (jwt/unsign token
                                       (env :secret)))
                 :user)))
