(ns clj-money.models.lot-transactions
  (:require [stowaway.core :as storage :refer [with-storage]]
            [clj-money.models :as models]))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (storage/select s (storage/tag criteria ::models/lot-transaction) options))))

(defn create
  [storage-spec lot-transaction]
  {:pre [(:transaction-id lot-transaction)
         (:transaction-date lot-transaction)]}

  (with-storage [s storage-spec]
    (storage/create s (storage/tag lot-transaction ::models/lot-transaction))))

(defn delete
  [storage-spec lot-transaction]
  (with-storage [s storage-spec]
    (storage/delete s (storage/tag lot-transaction ::models/lot-transaction))))
