(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id]]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :transaction/entity)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :transaction/entity)

(defmethod sql/prepare-criteria :transaction
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/deconstruct :transaction
  [{:transaction/keys [items transaction-date] :as trx}]
  (let [trx-id (or (:id trx)
                   (temp-id))]
    (cons (dissoc trx :transaction/items)
        (map #(assoc %
                     :transaction-item/transaction-id trx-id
                     :transaction-item/transaction-date transaction-date)
             items))))

(defmethod sql/before-save :transaction
  [trx]
  (->sql-refs trx))

(defmethod sql/after-read :transaction
  [trx]
  (->model-refs trx))
