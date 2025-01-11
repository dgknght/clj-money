(ns clj-money.models.settings
  (:refer-clojure :exclude [get])
  (:require [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]
            [honey.sql :as sql]
            [honey.sql.helpers :refer [select from where]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :refer [insert! update!]]
            [clj-money.core]))

(def ^:private settings-cache (atom {}))

(defn- db-config []
  (get-in env [:db :strategies :sql]))

(defn- ds []
  (jdbc/get-datasource (db-config)))

(defn- same-as-cached?
  [setting-name value]
  (when-let [retrieved (get-in @settings-cache [setting-name])]
    (= value retrieved)))

(defn- get*
  [setting-name]
  (jdbc/execute-one! (ds)
                     (-> (select :value)
                         (from :settings)
                         (where [:= :name setting-name])
                         sql/format)
                     jdbc/unqualified-snake-kebab-opts))

(defn- put*
  [setting-name value]
  (let [ds (ds)
        {::jdbc/keys [update-count]} (update! ds
                                              :settings
                                              {:value value}
                                              {:name setting-name})]
    (when (= 0 update-count)
      (insert! ds
               :settings
               {:name setting-name
                :value value}))))

(defn put
  [setting-name value]
  {:pre [(keyword? setting-name)]}

  (when-not (same-as-cached? setting-name value)
    (swap! settings-cache dissoc setting-name)
    (put* (name setting-name)
          (pr-str value))))

(defn- encache
  [v k]
  (swap! settings-cache assoc k v)
  v)

(defn get
  [setting-name]
  {:pre [(keyword? setting-name)]}

  (if-let [value (get-in @settings-cache [setting-name])]
    value
    (some-> (get* (name setting-name))
            :value
            read-string
            (encache setting-name))))
