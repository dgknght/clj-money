(ns clj-money.db.sql.budget-items
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [->json
                                            json->map]])
  (:import org.postgresql.jdbc.PgArray))


(defn- ->array
  [coll]
  (when (seq coll)
    (into-array java.math.BigDecimal coll)))

(defmethod sql/before-save :budget-item
  [budget-item]
  (-> budget-item
      (update-in [:budget-item/periods] ->array)
      (update-in [:budget-item/spec] ->json)))

(defmethod sql/resolve-temp-ids :budget-item
  [budget-item id-map]
  (update-in budget-item [:budget-item/budget-id] id-map))

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
      (update-in [:budget-item/spec] json->map)
      (update-in [:budget-item/periods] extract-bigdec-array)
      (update-in-if [:budget-item/spec :average] bigdec)
      (dissoc :budget-item/budget-id)))
