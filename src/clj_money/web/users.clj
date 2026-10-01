(ns clj-money.web.users
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.web.system :as system]
            [clj-money.web.auth :refer [read-token]]
            [clj-money.db :refer [unserialize-id]]
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
          (read-token (-> req (system/component :services) :auth-secret))
          :user-id
          unserialize-id
          entities/find))
