(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id?]]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :transaction-item/account :transaction-item/transaction)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :transaction-item/account :transaction-item/transaction)

(defmethod sql/resolve-temp-ids :transaction-item
  [{:transaction-item/keys [transaction-id] :as item} id-map]
  (if (temp-id? transaction-id)
    (update-in item [:transaction-item/transaction-id] id-map)
    item))

(defmethod sql/prepare-criteria :transaction-item
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/before-save :transaction-item
  [item]
  (-> item
      (update-in [:transaction-item/action] name)
      (->sql-refs)))

(defmethod sql/after-read :transaction-item
  [item]
  (-> item
      (update-in [:transaction-item/action] keyword)
      (->model-refs)))
