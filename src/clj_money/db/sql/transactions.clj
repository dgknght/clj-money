(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id]]))

(defmethod sql/deconstruct :transaction
  [{:transaction/keys [items transaction-date] :as trx}]
  (let [trx-id (or (:id trx)
                   (temp-id))]
    (cons (-> trx
              (assoc :id trx-id)
              (dissoc :transaction/items))
          (map #(assoc %
                       :transaction-item/transaction-id trx-id
                       :transaction-item/transaction-date transaction-date)
               items))))

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
