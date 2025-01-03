(ns clj-money.db.sql.accounts
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if parse-int]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [->json
                                            json->map]])
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

(defmethod sql/model-keys :account [_]
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
   :account/earliest-transaction-date
   :account/latest-transaction-date
   :account/created-at
   :account/updated-at])

(defmethod sql/before-save :account
  [account]
  (-> account
      (update-in [:account/type] name)
      (update-in [:account/system-tags] ->array)
      (update-in [:account/user-tags] ->array)
      (update-in-if [:account/allocations] ->json)))

(defn- parse-allocations
  [allocations]
  (when allocations
    (update-vals (json->map allocations :key-fn parse-int)
                 bigdec)))

(defmethod sql/after-read :account
  [account]
  (-> account
      (update-in [:account/type] keyword)
      (update-in [:account/system-tags] ->keyword-set)
      (update-in [:account/user-tags] ->keyword-set)
      (update-in [:account/allocations] parse-allocations)
      (update-in-if [:account/earliest-transaction-date] t/local-date)
      (update-in-if [:account/latest-transaction-date] t/local-date)))
