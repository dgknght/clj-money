(ns clj-money.db.datomic.transactions
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.dates :as dates]
            [clj-money.util :refer [+id temp-id]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :transaction
  [{:transaction/keys [lot-items] :as trx}]
  (cons (dissoc trx :transaction/lot-items)
        (map #(assoc % :lot-item/transaction trx)
             lot-items)))

(defmethod datomic/before-save :transaction
  [trx]
  (update-in trx
             [:transaction/items]
             (fn [items]
               (mapv #(+id % temp-id) items))))

(defmethod datomic/after-read :transaction
  [trx]
  (update-in trx [:transaction/transaction-date] dates/->local-date))
