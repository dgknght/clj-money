(ns clj-money.models.lot-transactions
  (:require [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.models :as models]))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (storage/select (tag criteria ::models/lot-transaction) options))))

(defn create
  [lot-transaction]
  {:pre [(:transaction-id lot-transaction)
         (:transaction-date lot-transaction)]}

  (with-storage (env :db)
    (storage/create (tag lot-transaction ::models/lot-transaction))))

(defn delete
  [lot-transaction]
  (with-storage (env :db)
    (storage/delete (tag lot-transaction ::models/lot-transaction))))
