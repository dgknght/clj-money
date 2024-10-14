(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :transaction-item/account)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :transaction-item/account)

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
