(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :transaction-item/account :transaction-item/transaction)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :transaction-item/account :transaction-item/transaction)

(defmethod sql/resolve-temp-ids :transaction-item
  [item id-map]
  (update-in item [:transaction-item/transaction-id] id-map))

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
