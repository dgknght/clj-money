(ns clj-money.db.sql.accounts
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if parse-int]]
            [clj-money.db.sql :as sql])
  (:import org.postgresql.jdbc.PgArray))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :account/entity :account/commodity :account/parent)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :account/entity :account/commodity :account/parent)

(defmethod sql/prepare-criteria :account
  [criteria]
  (criteria/apply-to criteria (comp ->sql-refs
                                    #(update-in-if % [:account/type] name))))

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

(defmethod sql/before-save :account
  [account]
  (-> account
      (update-in [:account/type] name)
      (update-in [:account/system-tags] ->array)
      (update-in [:account/user-tags] ->array)
      (update-in-if [:account/allocations] sql/->json)
      ->sql-refs))

(defn- parse-allocations
  [allocations]
  (when allocations
    (update-vals (sql/json->map allocations :key-fn parse-int)
                 bigdec)))

(defmethod sql/after-read :account
  [account]
  (-> account
      (update-in [:account/type] keyword)
      (update-in [:account/system-tags] ->keyword-set)
      (update-in [:account/user-tags] ->keyword-set)
      (update-in [:account/allocations] parse-allocations)
      ->model-refs))
