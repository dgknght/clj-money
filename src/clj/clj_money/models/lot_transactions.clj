(ns clj-money.models.lot-transactions
  (:require [stowaway.core :as storage :refer [with-storage]]
            [clj-money.models :as models]))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (storage/select s (models/tag criteria ::models/lot-transaction) options))))

(defn create
  [storage-spec lot-transaction]
  (with-storage [s storage-spec]
    (storage/create s (models/tag lot-transaction ::models/lot-transaction))))

(defn delete
  [storage-spec lot-transaction]
  (with-storage [s storage-spec]
    (storage/delete s (models/tag lot-transaction ::models/lot-transaction))))
