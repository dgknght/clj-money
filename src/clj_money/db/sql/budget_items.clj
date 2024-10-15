(ns clj-money.db.sql.budget-items
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :budget-item/account)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :budget-item/account)

(defmethod sql/prepare-criteria :budget-item
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defn- ->array
  [coll]
  (when (seq coll)
    (into-array java.math.BigDecimal coll)))

(defmethod sql/before-save :budget-item
  [budget-item]
  (-> budget-item
      (update-in [:budget-item/periods] ->array)
      (->sql-refs)))

(defmethod sql/resolve-temp-ids :budget-item-item
  [budget-item-item id-map]
  (update-in budget-item-item [:budget-item-item/budget-item-id] id-map))

(defmethod sql/after-read :budget-item
  [budget-item]
  (->model-refs budget-item))
