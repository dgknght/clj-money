(ns clj-money.db.sql.users
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db.sql :as sql])
  (:import org.postgresql.jdbc.PgArray))

(defmulti ^:private coerce-timestamp type)

(defmethod coerce-timestamp ::util/vector
  [[oper & vs]]
  (apply vector oper (map coerce-timestamp vs)))

(defmethod coerce-timestamp :default
  [v]
  (t/instant->sql-timestamp v))

(defn- coerce-types
  [user]
  (update-in-if user [:user/token-expires-at] coerce-timestamp))

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

(defmethod sql/before-save :user
  [user]
  (-> user
      coerce-types
      (dissoc :user/password-confirmation)
      (update-in-if [:user/roles] ->array)))

(defmethod sql/after-read :user
  [user]
  (update-in user [:user/roles] ->keyword-set))
