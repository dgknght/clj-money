(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare :as p]
            [next.jdbc.date-time]
            [clj-money.util :refer [temp-id?]])
  (:import org.postgresql.util.PGobject
           [java.sql Array PreparedStatement]))

(derive java.lang.Integer ::integer)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)
(derive java.util.UUID ::uuid)
(derive clojure.lang.PersistentVector ::vector)
(derive ::integer ::id)
(derive ::uuid ::id)

(next.jdbc.date-time/read-as-local)

(defn- map->pg-object
  [m]
  (doto (PGobject.)
    (.setType "jsonb")
    (.setValue (json/generate-string m))))

(defn- <-pg-object
  [^PGobject obj]
  (case (.getType obj)

    ("jsonb" "json")
    (json/parse-string (.getValue obj)
                       (fn [k]
                         ; Account use account ids as keys for the allocations
                         (if (re-matches #"\d+" k)
                           (Integer/parseInt k)
                           (keyword k))))

    (.getValue obj)))

(extend-protocol rs/ReadableColumn
  Array
  (read-column-by-label [^Array v _] (vec (.getArray v)))
  (read-column-by-index [^Array v _ _] (vec (.getArray v)))

  PGobject
  (read-column-by-label [^PGobject v _] (<-pg-object v))
  (read-column-by-index [^PGobject v _ _] (<-pg-object v)))

(extend-protocol p/SettableParameter
  clojure.lang.Keyword
  (set-parameter [^clojure.lang.Keyword k ^PreparedStatement s ^long i]
    (.setObject s i (name k)))

  clojure.lang.PersistentArrayMap
  (set-parameter [^clojure.lang.PersistentArrayMap m ^PreparedStatement s ^long i]
    (.setObject s i (map->pg-object m)))

  clojure.lang.PersistentHashMap
  (set-parameter [^clojure.lang.PersistentHashMap m ^PreparedStatement s ^long i]
    (.setObject s i (map->pg-object m))))

(defmulti coerce-id type)

(defmethod coerce-id ::id [id] id)

(defmethod coerce-id ::string
  [s]
  (cond
    (temp-id? s)            s
    (re-find #"^[0-9]+$" s) (parse-long s)
    :else                   (java.util.UUID/fromString s)))

(defmethod coerce-id ::vector
  [v]
  (mapv (fn [x]
          (if (string? x)
            (coerce-id x)
            x))
        v))
