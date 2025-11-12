(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare :as p]
            [next.jdbc.date-time])
  (:import org.postgresql.util.PGobject
           [java.sql Array Connection ParameterMetaData PreparedStatement]))

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

(defn- parameter-meta
  [^PreparedStatement stmt]
  (.getParameterMetaData stmt))

(defn- get-parameter-type-name
  [^ParameterMetaData meta ^long index]
  (.getParameterTypeName meta index))

(defn- extract-parameter-type
  [^PreparedStatement stmt
   ^long index]
  (let [meta (parameter-meta stmt)
        type-name (get-parameter-type-name meta index)]
    (when (str/starts-with? type-name "_")
      (apply str (rest type-name)))))

(defn- ->array
  [^clojure.lang.PersistentVector value
   ^PreparedStatement stmt
   ^long index]
  (when-let [type-name (extract-parameter-type stmt index)]
    (let [^Connection conn (.getConnection stmt)]
      (.createArrayOf conn type-name (to-array value)))))

(extend-protocol p/SettableParameter
  clojure.lang.Keyword
  (set-parameter [^clojure.lang.Keyword k ^PreparedStatement s ^long i]
    (.setObject s i (name k)))

  clojure.lang.PersistentVector
  (set-parameter [^clojure.lang.PersistentVector v ^PreparedStatement s ^long i]
    (.setObject s i (or (->array v s i) v)))

  clojure.lang.PersistentArrayMap
  (set-parameter [^clojure.lang.PersistentArrayMap m ^PreparedStatement s ^long i]
    (.setObject s i (map->pg-object m)))

  clojure.lang.PersistentHashMap
  (set-parameter [^clojure.lang.PersistentHashMap m ^PreparedStatement s ^long i]
    (.setObject s i (map->pg-object m))))

(defn- parse-uuid-id
  [x]
  (when (re-find #"\A[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}\z" x)
    (parse-uuid x)))

(defn- parse-int-id
  [x]
  (when-let [match (re-find #"\A(\d+)(:[a-z]+)?\z" x)]
    (parse-long (nth match 1))))

(def ^:private parse-id
  (some-fn parse-uuid-id
           parse-int-id
           identity))

(defn coerce-id
  [x]
  (cond
    (vector? x) (mapv coerce-id x)
    (string? x) (parse-id x)
    :else       x))
