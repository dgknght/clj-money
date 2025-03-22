(ns clj-money.db.sql.budget-items
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :refer [temp-id?]]
            [clj-money.db.sql :as sql])
  (:import org.postgresql.jdbc.PgArray))


(defn- ->array
  [coll]
  (when (seq coll)
    (into-array java.math.BigDecimal coll)))

(defmethod sql/before-save :budget-item
  [budget-item]
  (-> budget-item
      (update-in [:budget-item/periods] ->array)))

(defmethod sql/resolve-temp-ids :budget-item
  [{:as budget-item :budget-item/keys [budget-id]} id-map]
  (cond-> budget-item
    (temp-id? budget-id) (update-in [:budget-item/budget-id] id-map)))

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
      (update-in [:budget-item/periods] extract-bigdec-array)
      (update-in-if [:budget-item/spec :average] bigdec)
      (update-in-if [:budget-item/spec :total] bigdec)
      (update-in-if [:budget-item/spec :per-week] bigdec)
      (dissoc :budget-item/budget-id)))
