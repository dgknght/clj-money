(ns clj-money.models.settings
  (:refer-clojure :exclude [update get])
  (:require [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]))

(defn- after-read
  [setting]
  (-> setting
      (update-in [:value] read-string)
      (tag ::models/setting)))

(defn search
  [criteria options]
  (with-storage (env :db)
    (map after-read
         (storage/select (tag criteria ::models/setting)
                         options))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn- before-save
  [setting]
  (-> setting
      (update-in [:value] pr-str)
      (tag ::models/setting)))

(defn create
  [setting]
  {:pre [(:name setting) (:value setting)]}
  (with-storage (env :db)
    (-> setting
        before-save
        storage/create
        after-read)))

(defn update
  [setting]
  {:pre [(:name setting) (:value setting)]}
  (with-storage (env :db)
    (storage/update (before-save setting))
    (find-by {:name (:name setting)})))

(def ^:private settings-cache (atom {}))

(defn- same-as-cached?
  [setting-name value]
  (when-let [retrieved (get-in @settings-cache [setting-name])]
    (= value retrieved)))

(defn- put*
  [setting-name value]
  (with-storage (env :db)
    (if-let [existing (find-by {:name setting-name})]
      (update (assoc existing :value value))
      (create {:name setting-name
               :value value}))))

(defn put
  [setting-name value]
  (when-not (same-as-cached? setting-name value)
    (swap! settings-cache dissoc setting-name)
    (put* setting-name value)))

(defn- get*
  [setting-name]
  (:value (find-by {:name setting-name})))

(defn- encache
  [v k]
  (swap! settings-cache assoc k v)
  v)

(defn get
  [setting-name]
  (if-let [value (get-in @settings-cache [setting-name])]
    value
    (-> (get* setting-name)
        (encache setting-name))))
