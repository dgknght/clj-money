(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/deconstruct :transaction
  [{:transaction/keys [items lot-items transaction-date] :keys [id] :as trx}]
  (cons (dissoc trx
                :transaction/items
                :transaction/lot-items)
        (concat (map #(assoc %
                             :transaction-item/transaction-id id
                             :transaction-item/transaction-date transaction-date)
                     items)
                (map #(assoc %
                             :transaction-lot-item/transaction-id id
                             :transaction-lot-item/transaction-date transaction-date)
                     lot-items))))

(defmethod sql/reconstruct :transaction
  [models]
  ; TODO: should we remove :transaction-item/transaction to be consistent with Datomic?
  (util/reconstruct {:parent? :transaction/description
                     :child? :transaction-item/action
                     :children-key :transaction/items}
                    models))

(defmethod sql/after-read :transaction
  [trx]
  (update-in trx [:transaction/transaction-date] t/local-date))

(defmethod sql/post-select :transaction
  [storage trxs]
  (map #(assoc %
               :transaction/items
               (vec (db/select storage {:transaction-item/transaction %} {})))
       trxs))
