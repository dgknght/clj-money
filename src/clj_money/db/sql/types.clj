(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk
                                  postwalk]]
            [clojure.spec.alpha :as s]
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

(defn qid
  ([entity-type]
   #(qid % entity-type))
  ([id entity-type]
   {:pre [(integer? id)
          (keyword? entity-type)
          (not (namespace entity-type))]}
   (->QualifiedID id entity-type)))

(def ^:private qualified-id?
  (partial instance? QualifiedID))

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
   (when id-or-entity
     (if (map? id-or-entity)
       (update-in id-or-entity
                  [:id]
                  qualify-id
                  (or entity-type
                      (util/entity-type id-or-entity)))
       (->QualifiedID id-or-entity entity-type)))))

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

(defn- sqlize*
  [{:keys [ref-keys]}]
  (fn [x]
    (cond
      (qualified-id? x)
      (.id x)

      (and (map-entry? x)
           (ref-keys (key x)))
      (-> x
          (update-in [0] #(keyword (namespace %)
                                   (str (name %) "-id")))
          (update-in [1] :id))

      :else x)))

(defn sqlize
  ([opts]
   #(sqlize % opts))
  ([entity opts]
   (prewalk (sqlize* opts)
            entity)))

(def ^:private id-entry?
  (every-pred map-entry?
              #(= :id (key %))))

(defn- generalize-id-entry
  [{:keys [entity-type]}]
  (fn [x]
    (when (id-entry? x)
      (update-in x [1] (qid entity-type)))))

(defn- generalize-ref-entry
  [{:keys [ref-keys]}]
  (fn [x]
    (when (map-entry? x)
      (when-let [entity-type (ref-keys (key x))]
        (update-in x [1] (comp (partial hash-map :id)
                               (qid entity-type)))))))

(defn- generalize-ref-key
  [_]
  (fn [x]
    (when (keyword? x)
      (when-let [match (re-find #"\A(.+)-id\z" (name x))]
        (keyword (namespace x)
                 (second match))))))

(defn- generalize*
  [opts]
  (some-fn (generalize-ref-key opts)
           (generalize-ref-entry opts)
           (generalize-id-entry opts)
           identity))

(s/def ::ref-key (s/or :simple keyword?
                       :named (s/tuple keyword? keyword?)))
(s/def ::ref-keys (s/coll-of ::ref-key))
(s/def ::generalize-opts (s/keys :opt-un [::ref-keys]))

(defn- infer-types
  "Given the :key-refs options, return a map of keys
  to entity types"
  [ks]
  (->> ks
       (map #(if (keyword? %)
               [% (-> % name keyword)]
               %))
       (into {})))

(defn generalize
  [entity opts]
  {:pre [(s/valid? ::generalize-opts opts)]}
  (postwalk (generalize* (-> opts
                             (update-in [:ref-keys] infer-types)
                             (assoc :entity-type (util/entity-type entity))))
            entity))
