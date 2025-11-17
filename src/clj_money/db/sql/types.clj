(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk]]
            [cheshire.core :as json]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare :as p]
            [next.jdbc.date-time]
            [clj-money.entities :as e]
            [clj-money.util :as util])
  (:import org.postgresql.util.PGobject
           [java.sql Array Connection ParameterMetaData PreparedStatement]))

(deftype QualifiedID [id entity-type]
  e/CompositeID
  (components [_] {:id id :entity-type entity-type})

  Object
  (toString [_] (format "%s:%s" (name entity-type) id))
  (equals [_ other]
    (and (instance? QualifiedID other)
         (= id (.id other))
         (= (.entity-type other)
            entity-type))))

; TODO: Also handle UUID values
(defn unserialize-id
  [^String s]
  (when-let [match (re-find #"\A(\d+)(:[a-z]+)?\z" s)]
    (->QualifiedID (parse-long (nth match 1))
          (keyword (nth match 2)))))

(defn qualify-id
  ([entity-or-type]
   (cond
     (map? entity-or-type)
     (update-in entity-or-type
                [:id]
                qualify-id
                (util/entity-type entity-or-type))

     (or (nil? entity-or-type)
         (keyword? entity-or-type))
     (fn [x]
       (qualify-id x entity-or-type))

     :else
     (throw (ex-info "Unrecognized entity-or-type" {:entity-or-type entity-or-type}))))
  ([id-or-entity entity-type]
   {:pre [id-or-entity
          (or entity-type
              (map? id-or-entity))]}
   (cond
     (map? id-or-entity)
     (update-in id-or-entity
                [:id]
                qualify-id
                (or entity-type
                    (util/entity-type id-or-entity)))

     :else
     (->QualifiedID id-or-entity entity-type))))

(defn unqualify-id
  [^QualifiedID qid]
  (.id qid))

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

(defn ->sql-ids
  [criteria]
  (prewalk (fn [x]
             (if (instance? QualifiedID x)
               (.id x)
               x))
           criteria))
