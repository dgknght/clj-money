(ns clj-money.db.sql
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [camel-snake-kebab.core :refer [->snake_case_keyword]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql.builder :refer [for-insert
                                           for-update
                                           for-delete]]
            [next.jdbc.date-time]
            [next.jdbc.result-set :as rs]
            [stowaway.criteria :as crt]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.inflection :refer [plural
                                                singular]]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql.queries :refer [criteria->query
                                              ->update]]
            [clj-money.db.sql.types :refer [temp-id?
                                            coerce-id]])
  (:import org.postgresql.util.PGobject
           java.sql.Array))

(extend-protocol rs/ReadableColumn
  Array
  (read-column-by-label [^Array v _] (vec (.getArray v)))
  (read-column-by-index [^Array v _ _] (vec (.getArray v))))

(defmulti deconstruct (fn [x]
                        (when-not (vector? x)
                          (db/model-type x))))
(defmethod deconstruct :default [m] [m])

(defmulti reconstruct
  (fn [ms]
    (when-let [m1 (first ms)]
      (db/model-type m1))))
(defmethod reconstruct :default [ms] ms)

(defmulti before-save db/type-dispatch)
(defmethod before-save :default [m] m)

(defmulti after-read db/model-type)
(defmethod after-read :default [m] m)

(defmulti prepare-criteria db/model-type)
(defmethod prepare-criteria :default [m] m)

(defmulti post-select
  (fn [_storage ms]
    (when-let [m1 (first ms)]
      (db/model-type m1))))
(defmethod post-select :default [_ ms] ms)

(def ^:private infer-table-name
  (comp ->snake_case_keyword
        plural
        db/model-type))

(defn- insert
  [db model]
  (let [table (infer-table-name model)
        s (for-insert table
                      model
                      jdbc/snake-kebab-opts)
        result (jdbc/execute-one! db s {:return-keys [:id]})]

    ; TODO: scrub for sensitive data
    (log/debugf "database insert %s -> %s" model s)
    (get-in result [(keyword (name table) "id")])))

(defn- update
  [db model]
  {:pre [(:id model)]}
  (let [table (infer-table-name model)
        s (for-update table
                      (dissoc model :id)
                      {:id (:id model)}
                      jdbc/snake-kebab-opts)
        result (jdbc/execute-one! db s {:return-keys [:id]})]

    ; TODO: scrub sensitive data
    (log/debugf "database update %s -> %s" model s)

    (get-in result [(keyword (name table) "id")])))

(defn delete-one
  [ds m]
  (let [s (for-delete (infer-table-name m)
                      {:id (:id m)} ; TODO: find the id attribute
                      {})]

    ; TODO: scrub sensitive data
    (log/debugf "database delete %s -> %s" m s)

    (jdbc/execute! ds s)
    1))

(defn- put-one
  [ds [oper model]]
  (case oper
    ::db/insert (insert ds model)
    ::db/update (update ds model)
    ::db/delete (delete-one ds model)
    (throw (ex-info "Invalid operation" {:operation oper}))))

(def ^:private id?
  (every-pred identity (complement temp-id?)))

(defn- wrap-oper
  "Ensure that what we are passing on is a tuple with a database
  operation in the 1st position and a model in the second."
  [{:as m :keys [id]}]
  (cond
    (vector? m) m
    (id? id)    [::db/update m]
    :else       [::db/insert m]))

(defmulti resolve-temp-ids
  "In a model-specific way, replace temporary ids with proper ids after a save."
  db/type-dispatch)

(defmethod resolve-temp-ids :default
  [model _id-map]
  model)

(defn- execute-and-aggregate
  "Returns a function that executes the database operation, saves the result
  and updates the id map for resolving temporary ids"
  [ds]
  (fn [{:as result :keys [id-map]} [operator m]]
    (let [id-resolved (cond-> m
                        (seq id-map) (resolve-temp-ids id-map)
                        (temp-id? m) (dissoc :id))
          saved (put-one ds [operator id-resolved])]
      (cond-> (update-in result [:saved] conj (assoc id-resolved :id saved))
        (temp-id? m)
        (assoc-in [:id-map (:id m)]
                  saved)))))

(s/def ::operation #{::db/insert ::db/update ::db/delete})
(s/def ::putable (s/or :map map?
                       :operation (s/tuple ::operation map?)))
(s/def ::putables (s/coll-of ::putable))

; This is only exposed publicly to support tests that enforce
; short-circuting transaction propagation
(defn put*
  "Executes operations against the database. This function is not entended
  to be used directly."
  [ds models]
  {:pre [(s/valid? ::putables models)]}
  (jdbc/with-transaction [tx ds]
    (->> models
         (mapcat deconstruct)
         (map (comp #(update-in % [1] before-save)
                    wrap-oper))
         (reduce (execute-and-aggregate tx)
                 {:saved []
                  :id-map {}})
         :saved
         (map after-read)
         (reconstruct))))

(defn- update*
  [ds changes criteria]
  (let [m-type (db/model-type changes)
        s (->update (before-save changes)
                    (-> criteria
                        (db/model-type m-type)
                        prepare-criteria)
                    {:target m-type})
        result (jdbc/execute-one! ds s)]

    ; TODO: scrub sensitive data
    (log/debugf "database update %s %s -> %s" criteria changes s)

    (:next.jdbc/update-count result)))

(defn- id-key
  [x]
  (when-let [target (db/model-type x)]
    (keyword (name target) "id")))

(defn- massage-ids
  "Coerces ids and appends the appropriate namespace
  to the :id key"
  [m]
  (let [k (id-key m)]
    (cond-> (crt/apply-to m #(update-in-if % [:id] coerce-id))
      k (rename-keys {:id k}))))

(defn- refine-qualifiers
  "Singularize qualifiers based on plural table names and strip
  the qualifier from the :id attribute"
  [m]
  (update-keys m #(let [q (namespace %)
                        k (name %)]
                    (if (= "id" k)
                      :id
                      (keyword (singular q) k)))))

(def ^:private recursions
  {:account [:parent-id :id]})

(defn- select*
  [ds criteria {:as options :keys [include-children?]}]
  (let [model-type (db/model-type criteria)
        query (-> criteria
                  (crt/apply-to massage-ids)
                  prepare-criteria
                  (criteria->query (cond-> (assoc options :target model-type)
                                     include-children? (assoc :recursion (recursions model-type)))))]

    ; TODO: scrub sensitive data
    (log/debugf "database select %s with options %s -> %s" criteria options query)

    (if (:count options)
      (jdbc/execute-one! ds
                         query
                         jdbc/unqualified-snake-kebab-opts)
      (post-select
        (:storage options)
        (map (comp after-read
                   refine-qualifiers)
             (jdbc/execute! ds
                            query
                            jdbc/snake-kebab-opts))))))

(defn- delete*
  [ds models]
  {:pre [(s/valid? (s/coll-of map?) models)]}

  (->> models
       (interleave (repeat ::db/delete))
       (partition 2)
       (map vec)
       (put* ds)))

(defn- reset*
  [ds]
  (jdbc/execute! ds ["truncate table cached_prices; truncate table users cascade"]))

(defmethod db/reify-storage ::db/sql
  [config]
  (let [ds (jdbc/get-datasource config)]
    (reify db/Storage
      (put [_ models] (put* ds models))
      (update [_ changes criteria] (update* ds changes criteria))
      (select [this criteria options] (select* ds criteria (assoc options :storage this)))
      (delete [_ models] (delete* ds models))
      (close [_]) ; this is a no-op for next-jdbc
      (reset [_] (reset* ds)))))

(defmacro def->sql-refs
  "Give a model map, covert the values at the specified keys into
  simple id values and add the -id suffix to the key"
  [fn-name & keys]
  (let [id-keys (mapv #(keyword (namespace %)
                                (str (name %) "-id"))
                      keys)
        key-map (zipmap keys id-keys)]
    `(defn- ~fn-name
       [model#]
       (reduce #(util/update-in-criteria %1 %2 ->id)
               (rename-keys model# ~key-map)
               ~id-keys))))

(defn ->model-ref
  ([x] (->model-ref x identity))
  ([x coerce]
   (if (map? x)
     (-> x
         (select-keys [:id])
         (update-in-if [:id] coerce))
     {:id (coerce x)})))

(defmacro def->model-refs
  "Given a model map, convert the values at the specified keys into
  model reference maps like {:id 123} and remove the -id suffix
  from the key"
  [fn-name & keys]
  (let [id-keys (mapv #(keyword (namespace %)
                                (str (name %) "-id"))
                      keys)
        key-map (zipmap id-keys keys)]
    `(defn- ~fn-name
       [model#]
       (rename-keys (reduce #(update-in-if %1 [%2] ->model-ref)
                            model#
                            ~id-keys)
                    ~key-map))))

(defn ->json
  [x]
  (when x
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (json/generate-string x)))))

(defn json->map
  [^org.postgresql.util.PGobject x & {:keys [key-fn] :or {key-fn true}}]
  (when x
    (json/parse-string (.getValue x) key-fn)))
