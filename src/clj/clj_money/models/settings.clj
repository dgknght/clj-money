(ns clj-money.models.settings
  (:refer-clojure :exclude [update get])
  (:require [stowaway.core :as storage :refer [with-storage]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]))

(defn- after-read
  [setting]
  (-> setting
      (update-in [:value] read-string)
      (storage/tag ::models/setting)))

(defn search
  [storage-spec criteria options]
  (with-storage [s storage-spec]
    (map after-read
         (storage/select s
                         (storage/tag criteria ::models/setting)
                         options))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (assoc options :limit 1)))))

(defn- before-save
  [setting]
  (-> setting
      (update-in [:value] pr-str)
      (storage/tag ::models/setting)))

(defn create
  [storage-spec setting]
  {:pre [(:name setting) (:value setting)]}
  (with-storage [s storage-spec]
    (-> (storage/create s (before-save setting))
        first
        after-read)))

(defn update
  [storage-spec setting]
  {:pre [(:name setting) (:value setting)]}
  (with-storage [s storage-spec]
    (storage/update s (before-save setting))
    (find-by s {:name (:name setting)})))

(def ^:private settings-cache (atom {}))

(defn- same-as-cached?
  [setting-name value]
  (when-let [retrieved (get-in @settings-cache [setting-name])]
    (= value retrieved)))

(defn- put*
  [storage-spec setting-name value]
  (with-storage [s storage-spec]
    (if-let [existing (find-by s {:name setting-name})]
      (update storage-spec (assoc existing :value value))
      (create storage-spec {:name setting-name
                            :value value}))))

(defn put
  [storage-spec setting-name value]
  (when-not (same-as-cached? setting-name value)
    (swap! settings-cache dissoc setting-name)
    (put* storage-spec setting-name value)))

(defn- get*
  [storage-spec setting-name]
  (:value (find-by storage-spec {:name setting-name})))

(defn get
  [storage-spec setting-name]
  (if-let [value (get-in @settings-cache [setting-name])]
    value
    (let [retrieved (get* storage-spec setting-name)]
      (swap! settings-cache assoc setting-name retrieved)
      retrieved)))
