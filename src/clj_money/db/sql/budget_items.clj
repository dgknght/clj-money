(ns clj-money.db.sql.budget-items
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql])
  (:import org.postgresql.jdbc.PgArray))

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
      (update-in [:budget-item/spec] sql/->json)
      (->sql-refs)))

(defmethod sql/resolve-temp-ids :budget-item-item
  [budget-item-item id-map]
  (update-in budget-item-item [:budget-item-item/budget-item-id] id-map))

(defmulti ^:private extract-bigdec-array type)

(defmethod extract-bigdec-array :default
  [x]
  (into [] x))

(defmethod extract-bigdec-array PgArray
  [^PgArray pg-arr]
  (into [] (.getArray pg-arr)))

(defmethod sql/after-read :budget-item
  [budget-item]
  (-> budget-item
      (->model-refs)
      (update-in [:budget-item/spec] sql/json->map)
      (update-in [:budget-item/periods] extract-bigdec-array)
      (update-in-if [:budget-item/spec :average] bigdec)
      (dissoc :budget-item/budget-id)))
