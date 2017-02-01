(ns clj-money.models.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-commodity
                                              select-commodities-by-entity-id]]))

(defn- before-save
  [commodity]
  (update-in commodity [:exchange] name))

(defn- after-read
  [commodity]
  (update-in commodity [:exchange] keyword))

(defn create
  "Creates a new commodity record"
  [storage-spec commodity]
  (with-storage [s storage-spec]
    (->> commodity
         before-save
         (create-commodity s)
         after-read)))

(defn select-by-entity-id
  "Returns the commodities belonging to the specified entity"
  [storage-spec entity-id]
  (with-storage [s storage-spec]
    (->> entity-id
         (select-commodities-by-entity-id s)
         (map after-read))))
