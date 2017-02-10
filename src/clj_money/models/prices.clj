(ns clj-money.models.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-price
                                              select-prices-by-commodity-id]]))

(defn- before-save
  [price]
  (update-in price [:trade-date] tc/to-long))

(defn- after-read
  [price]
  (update-in price [:trade-date] tc/to-local-date))

(defn create
  [storage-spec price]
  (with-storage [s storage-spec]
    (->> price
         before-save
         (create-price s)
         after-read)))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  [])
