(ns clj-money.models.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-lot
                                              select-lots-by-commodity-id]]))

(defn- before-save
  [lot]
  (-> lot
      (update-in [:purchase-date] tc/to-long)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- after-read
  [lot]
  (update-in lot [:purchase-date] tc/to-local-date))

(defn create
  [storage-spec lot]
  (with-storage [s storage-spec]
    (->> lot
         before-save
         (create-lot s )
         after-read)))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  (with-storage [s storage-spec]
    (->> commodity-id
         (select-lots-by-commodity-id s)
         (map after-read))))
