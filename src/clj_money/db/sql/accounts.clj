(ns clj-money.db.sql.accounts
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types])
  (:import org.postgresql.jdbc.PgArray))

(defn- ->array
  [coll]
  (when (seq coll)
    (into-array (map name coll))))

(defmulti ^:private ->keyword-set type)

(defmethod ->keyword-set :default
  [coll]
  (when coll
    (->> coll
         seq
         (map keyword)
         set)))

(defmethod ->keyword-set PgArray
  [^PgArray pg-arr]
  (->keyword-set (.getArray pg-arr)))

(defmethod sql/entity-keys :account [_]
  [:id
   :account/name
   :account/type
   :account/entity-id
   :account/commodity-id
   :account/parent-id
   :account/quantity
   :account/value
   :account/system-tags
   :account/user-tags
   :account/allocations
   :account/price-as-of
   :account/transaction-date-range
   :account/created-at
   :account/updated-at])

(defmethod sql/before-save :account
  [account]
  (-> account
      (update-in [:account/type] name)
      (update-in [:account/system-tags] ->array)
      (update-in [:account/user-tags] ->array)))

(defn- parse-allocations
  [allocations]
  (some-> allocations
          (update-vals bigdec)
          (update-keys (types/qid :account))))

(defmethod sql/after-read :account
  [account]
  (-> account
      (update-in [:account/type] keyword)
      (update-in [:account/system-tags] ->keyword-set)
      (update-in [:account/user-tags] ->keyword-set)
      (update-in [:account/allocations] parse-allocations)
      (update-in-if [:account/transaction-date-range 0] t/local-date)
      (update-in-if [:account/transaction-date-range 1] t/local-date)))
