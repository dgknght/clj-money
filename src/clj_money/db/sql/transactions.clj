(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id]]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :transaction/entity :transaction/scheduled-transaction)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :transaction/entity :transaction/scheduled-transaction)

(defmethod sql/prepare-criteria :transaction
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

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

(defmethod sql/before-save :transaction
  [trx]
  (->sql-refs trx))

(defmethod sql/after-read :transaction
  [trx]
  (-> trx
      (update-in [:transaction/transaction-date] t/local-date)
      (->model-refs)))

(defmethod sql/post-select :transaction
  [storage trxs]
  (map #(assoc %
               :transaction/items
               (vec (db/select storage {:transaction-item/transaction %} {})))
       trxs))
