(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk
                                  postwalk]]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder]]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare :as p]
            [next.jdbc.date-time]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db :as db]
            [clj-money.entities :as e]
            [clj-money.util :as util])
  (:import org.postgresql.util.PGobject
           [java.sql Array Connection ParameterMetaData PreparedStatement]))

(deftype QualifiedID [id entity-type]
  e/CompositeID
  (components [_] {:id id :entity-type entity-type})

  Object
  (toString [_] (format "%s:%s" id (name entity-type)))
  (hashCode [this] (.hashCode (.toString this)))
  (equals [_ other]
    (and (instance? QualifiedID other)
         (= id (.id other))
         (= (.entity-type other)
            entity-type))))

(def qualified-id?
  (partial instance? QualifiedID))

(def qid? qualified-id?)

(defn qid
  ([entity-type]
   #(qid % entity-type))
  ([id entity-type]
   {:pre [(or (qid? id)
              (integer? id)
              (uuid? id))
          (keyword? entity-type)
          (not (namespace entity-type))]}
   (if (qid? id)
     id
     (->QualifiedID id entity-type))))

(defn parse-qid
  [s]
  (let [[id entity-type] (str/split s #":")]
    (qid (parse-long id) ; TODO: Might this be a uuid?
         (keyword entity-type))))

(defmethod print-method QualifiedID
  [this ^java.io.Writer w]
  (doto w
    (.write "#clj-money/qid \"")
    (.write (str this))
    (.write "\"")))

(add-encoder
  QualifiedID
  (fn [id gen]
    (.writeString gen (str id))))

; TODO: Also handle UUID values
(defn unserialize-qid
  [s]
  (when-let [match (when (string? s)
                     (re-find #"\A(\d+)(?::([a-z\-]+))?\z" s))]
    (->QualifiedID (parse-long (nth match 1))
          (keyword (nth match 2)))))

(db/register-id-unserializer unserialize-qid)

(def ^:private long-pattern #"\A\d+\z")

(defn- unserialize-integer-id
  [s]
  (when-let [match (when (string? s)
                     (re-find long-pattern s))]
    (parse-long (first match))))

(db/register-id-unserializer unserialize-integer-id)

(def ^:private uuid-pattern #"\A[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}\z")

(defn- unserialize-uuid
  [s]
  (when-let [match (when (string? s)
                     (re-find uuid-pattern s))]
    (parse-uuid (first match))))

(db/register-id-unserializer unserialize-uuid)

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

(defn- ref-entry?
  [ref-keys]
  (every-pred map-entry?
              (comp ref-keys key)))

(defn- sqlize-ref-key
  [plural?]
  (fn
    [k]
    (if plural?
      (keyword (namespace k)
               (str (str/replace 
                      (name k)
                      #"s\z"
                      "") "-ids"))
      (keyword (namespace k)
               (str (name k) "-id")))))

(defn- sqlize-ref-val
  [v]
  (cond
    (vector? v) ; criterion with operator, like [:in `(1 2 3)], or a plural value like {:import/images [{:id 1} {:id 2}]}
    (if (keyword? (first v))
      [(first v)
       (if (sequential? (second v))
         (map sqlize-ref-val (second v)) ; e.g. [:in '({:id 101} {:id 102})]
         (sqlize-ref-val (second v)))]   ; e.g. [:!= {:id 101}]
      (mapv sqlize-ref-val v))

    (map? v) ; an entity or a reference
    (:id v)

    :else v))

; Entities:
; - {:entity/user {:id 1}} -> {:entity/user-id 1}
; - {:import/images [{:id 1} {:id 2}]} -> {:import/image-ids [1 2]}
; Criteria:
; - {:budget-item/budget [:in '({:id 1} {:id 2})]} -> {:budget-item/budget-id [:in '(1 2)]}
(defn- sqlize-ref-entry
  [ref-keys]
  (fn [entry]
    (let [type-spec (ref-keys (key entry))]
      (-> entry
          (update-in [0] (sqlize-ref-key (vector? type-spec)))
          (update-in [1] sqlize-ref-val)))))

(defn- sqlize*
  [{:keys [ref-keys]}]
  (fn [x]
    (cond
      (qualified-id? x)
      (.id x)

      ((ref-entry? ref-keys) x)
      ((sqlize-ref-entry ref-keys) x)

      (and (map-entry? x)
           (keyword (val x)))
      (update-in x [1] name)

      :else x)))

(defn- normalize-sqlize-ref-keys
  [ks]
  (->> ks
       (map #(if (keyword? %)
               [% :singular]
               %))
       (into {})))

(defn- normalize-sqlize-opts
  [opts]
  (update-in-if opts [:ref-keys] normalize-sqlize-ref-keys))

(s/def ::ref-key (s/or :implicit keyword?
                       :explicit (s/tuple keyword?
                                          (s/or :singular keyword?
                                                :plural (s/tuple keyword?)))))
(s/def ::ref-keys (s/coll-of ::ref-key))
(s/def ::sqlize-options (s/keys :req-un [::ref-keys]))

(defn sqlize
  "Given a domain entity map, criteria, or change set, adjust keys and values
  for SQL storage.

  The option :ref-keys is a list of keys that have entity reference values.
  E.g. #{:entity/user}.

  The :ref-keys can also include the type of the value, especially when
  necessary to indicate plurality.
  Elg. #{:import/user
         [:import/images [:image]]}"
  ([opts]
   {:pre [(s/valid? ::sqlize-options opts)]}
   #(sqlize % opts))
  ([x opts]
   {:pre [(s/valid? ::sqlize-options opts)]}
   (let [options (normalize-sqlize-opts opts)]
     (prewalk (sqlize* options)
              x))))

(def ^:private id-entry?
  (every-pred map-entry?
              #(= :id (key %))))

(defn- generalize-id-entry
  [{:keys [entity-type]}]
  (fn [x]
    (when (and (id-entry? x)
               (not (qualified-id? (val x))))
      (update-in x [1] (qid entity-type)))))

(defn- generalize-ref-key
  [k]
  (keyword (namespace k)
           (str/replace (name k)
                        #"-id(s)?\z"
                        (fn [m]
                          (or (second m)
                              "")))))

(defn- generalize-ref-val
  ([entity-type] #(generalize-ref-val % entity-type))
  ([v entity-type]
   (cond
     (sequential? v)
     (mapv #(generalize-ref-val % entity-type) v)

     v
     {:id (qid v entity-type)})))

(defn- generalize-ref-entry
  [{:keys [sql-ref-keys]}]
  (fn [x]
    (when (map-entry? x)
      (when-let [entity-type (sql-ref-keys (key x))]
        (-> x
            (update-in [0] generalize-ref-key)
            (update-in [1] (generalize-ref-val entity-type)))))))

(defn- generalize*
  [opts]
  (some-fn (generalize-ref-entry opts)
           (generalize-id-entry opts)
           identity))

(s/def ::sql-ref-key (s/or :simple keyword?
                           :named (s/tuple keyword? keyword?)))
(s/def ::sql-ref-keys (s/or :implicit (s/coll-of ::sql-ref-key)
                            :explicit (s/map-of keyword? keyword?)))
(s/def ::generalize-opts (s/keys :req-un [::sql-ref-keys]))

(defn- infer-types
  "Given the :key-refs options, return a map of keys
  to entity types"
  [ks]
  (if (map? ks)
    ks
    (->> ks
         (map (fn [x]
                (if (keyword? x)
                  [x (->> x
                          name
                          (re-find #"\A.+(?=-id\z)")
                          keyword)]
                  x)))
         (into {}))))

(defn generalize
  "Given a map as read from a SQL database, convert it to a generalized
  map for the application domain.

  The option :ref-keys is a list of:
    - An attribute key. E.g. :entity/user-id
    - An attribute key and type. E.g. [:account/parent-id :account]
  Or it can be a map of attribute keys to types. E.g.
    {:account/parent-id :account}"
  [entity opts]
  {:pre [(s/valid? ::generalize-opts opts)]}
  (postwalk (generalize* (-> opts
                             (update-in [:sql-ref-keys] infer-types)
                             (assoc :entity-type (util/entity-type entity))))
            entity))
