(ns clj-money.db.sql
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [prewalk]]
            [camel-snake-kebab.core :refer [->snake_case_keyword
                                            ->snake_case_string
                                            ->snake_case
                                            ->kebab-case]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql.builder :refer [for-insert
                                           for-update
                                           for-delete]]
            [next.jdbc.date-time]
            [next.jdbc.result-set :as result-set]
            [stowaway.criteria :as crt]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util :refer [temp-id?
                                             live-id?]]
            [clj-money.entities :as entities]
            [clj-money.entities.schema :as schema]
            [clj-money.db :as db]
            [clj-money.db.sql.types :as types]
            [clj-money.db.sql.queries :refer [criteria->query
                                              ->update]])
  (:import clj_money.db.sql.types.QualifiedID))

(defmulti deconstruct (fn [x]
                        (when-not (vector? x)
                          (util/entity-type x))))
(defmethod deconstruct :default [m] [m])

(defmulti before-save util/entity-type-dispatch)
(defmethod before-save :default [m] m)

(defmulti after-read util/entity-type-dispatch)
(defmethod after-read :default [m] m)

(defmulti entity-keys util/entity-type-dispatch)
(defmethod entity-keys :default [_] [])

(defn- append-qualifiers
  [[entity-type :as entry]]
  (update-in entry
             [1]
             (partial map (fn [k]
                            (if (= :id k)
                              k
                              (keyword (name entity-type)
                                       (name k)))))))

(def ^:private primary-keys
  (->> schema/entities
       (filter :primary-key)
       (map (comp append-qualifiers
                  (juxt :id :primary-key)))
       (into {})))

(def ^:private reconstruction-rules
  {:budget [{:parent? :budget/name
             :child? :budget-item/account
             :children-key :budget/items}]
   :reconciliation [{:parent? :reconciliation/end-of-period
                     :child? :transaction-item/reconciliation
                     :children-key :reconciliation/items}]
   :scheduled-transaction [{:parent? :scheduled-transaction/description
                            :child? :scheduled-transaction-item/action
                            :children-key :scheduled-transaction/items}]
   :transaction [{:parent? :transaction/description
                  :child? :transaction-item/action
                  :children-key :transaction/items}
                 {:parent? :transaction/description
                  :child? :lot-item/action
                  :children-key :transaction/lot-items}]})

(defn- scrub-values
  [m [stmt & args]]
  (let [get-key (map-invert m)]
    (vec (cons stmt
               (map (fn [arg]
                      (if (entities/sensitive-keys (get-key arg))
                        "********"
                        arg))
                    args)))))

(defn- reconstruct
  [entities]
  (if-let [rules (->> entities
                   (map util/entity-type)
                   set
                   (mapcat reconstruction-rules)
                   (filter identity)
                   seq)]
    (reduce (fn [ms rule-map]
              (util/reconstruct rule-map ms))
            entities
            rules)
    entities))

; post-read coercions
(def ^:private coercions
  {:transaction-item/action keyword})

(defn- apply-coercions
  [x]
  (reduce (fn [m [k f]]
            (update-in-if m [k] f))
          x
          coercions))

(def ^:private sql-ref-keys
  (mapv #(keyword (namespace %)
                  (str (name %) "-id"))
        schema/entity-ref-keys))

(def ^:private entity->sql-ref-map
  (zipmap schema/entity-ref-keys sql-ref-keys))

(defn- extract-ref-id
  [x]
  (cond
    ; An operation, like [:in '(1 2 3)]
    (vector? x)
    (apply vector (first x) (map extract-ref-id (rest x)))

    ; A entity or entity reference, like {:id 1}
    (map? x)
    (if-let [id (:id x)]
      (extract-ref-id id)
      x)

    ; A list of values in an :in clause, like [:in #{1 2 3}]
    (coll? x)
    (set (map extract-ref-id x))

    ; a value that can be used as-is
    :else x))

(defn- ->sql-refs
  [m]
  (reduce (fn [m k]
            (update-in-if m [k] extract-ref-id))
          (rename-keys m entity->sql-ref-map)
          sql-ref-keys))

(def ^:private sql->entity-ref-map
  (zipmap sql-ref-keys schema/entity-ref-keys))

(defn- ->entity-refs
  "Convert SQL foreign key columns into entity references.

  E.g.
  (->entity-refs {:entity/name \"Personal\" :entity/user-id 123}) =>
  {:entity/name \"Personal\" :entity/user {:id (->QualifiedID 123 :user)}} "
  [m]
  (let [renamed (rename-keys m sql->entity-ref-map)]
    (->> schema/entity-ref-keys
         (filter #(contains? renamed %))
         (reduce (fn [m k]
                   (update-in m
                              [k]
                              (comp util/->entity-ref
                                    (types/qualify-id (-> k name keyword)))))
                 renamed))))

(defmulti post-select
  (fn [_opts ms]
    (when-let [m1 (first ms)]
      (util/entity-type m1))))
(defmethod post-select :default [_ ms] ms)

(def ^:private infer-table-name
  (comp ->snake_case_keyword
        util/entity-type))

(defn- quote
  [x]
  (str "\"" x "\""))

(def ^:private sql-opts
  {:column-fn (comp quote ->snake_case)
   :table-fn (comp quote ->snake_case)
   :label-fn ->kebab-case
   :qualifier-fn ->kebab-case
   :builder-fn result-set/as-kebab-maps})

(defn- insert
  [db entity]
  (let [table (infer-table-name entity)
        s (for-insert table
                      entity
                      sql-opts)
        _ (log/debugf "database insert %s -> %s"
                      (entities/scrub-sensitive-data entity)
                      (scrub-values entity s))
        result (jdbc/execute-one! db s {:return-keys [:id]})]
    (get-in result [(keyword (name table) "id")])))

(defn- update
  [db entity]
  {:pre [(:id entity)]}

  (let [table (infer-table-name entity)
        s (for-update table
                      (dissoc entity :id)
                      {:id (:id entity)}
                      sql-opts)
        _ (log/debugf "database update %s -> %s"
                      (entities/scrub-sensitive-data entity)
                      (scrub-values entity s))
        result (jdbc/execute-one! db s {:return-keys [:id]})]

    (get-in result [(keyword (name table) "id")])))

(defn delete-one
  [ds m]
  (let [primary-key (primary-keys (util/entity-type m) [:id])
        s (for-delete (infer-table-name m)
                      (select-keys m primary-key)
                      sql-opts)]

    (log/debugf "database delete %s -> %s"
                (entities/scrub-sensitive-data m)
                s)

    (jdbc/execute! ds s)
    1))

(defn- put-one
  [ds [oper entity]]
  (case oper
    ::db/insert (insert ds entity)
    ::db/update (update ds entity)
    ::db/delete (delete-one ds entity)
    (throw (ex-info "Invalid operation" {:operation oper}))))

(defn- wrap-oper
  "Ensure that what we are passing on is a tuple with a database
  operation in the 1st position and a entity in the second."
  [{:as m :keys [id]}]
  (cond
    (vector? m)      m
    (live-id? id)    [::db/update m]
    :else            [::db/insert m]))

(defmulti resolve-temp-ids
  "In a entity-specific way, replace temporary ids with proper ids after a save."
  util/entity-type-dispatch)

(defmethod resolve-temp-ids :default
  [entity _id-map]
  entity)

(defn- ref-to-attrs
  [ref]
  (if (keyword? ref)
    [(str (name ref) "-id")]
    (let [{:keys [columns]} ref]
      (map (fn [c]
             (if (keyword? c)
               c
               (second c)))
           columns))))

(defn- build-attributes
  [[t fields refs]]
  (let [attrs (->> refs
                   (mapcat ref-to-attrs)
                   (map #(keyword (name t) (name %)))
                   (concat (map :id fields))
                   (map (comp #(keyword (name t) %)
                              name))
                   set)]
    [t (conj attrs :id)]))

(def attributes
  "A map of entity types to attributes for the type"
  (-> (->> schema/entities
           (map (comp build-attributes
                      (juxt :id :fields :refs)))
           (into {}))
      (update-in [:transaction-item]
                 conj
                 :transaction-item/transaction-date
                 :transaction-item/transaction-id)))

(defn- strip-unrecognized-keys
  [m]
  (select-keys m (attributes (util/entity-type m))))

(defn- execute-and-aggregate
  "Returns a function that executes the database operation, saves the result
  and updates the id map for resolving temporary ids"
  [ds]
  (fn [{:as result :keys [id-map]} [operator m]]
    (let [id-resolved (cond-> (strip-unrecognized-keys m)
                        (seq id-map) (resolve-temp-ids id-map)
                        (temp-id? m) (dissoc :id))
          saved (put-one ds [operator id-resolved])]
      (cond-> result
        (not= ::db/delete operator)
        (update-in [:saved] conj (assoc id-resolved :id saved))

        (temp-id? m)
        (assoc-in [:id-map (:id m)]
                  saved)))))

(s/def ::id (some-fn string?
                     integer?
                     uuid?))
(s/def ::entity (s/and (s/keys :req-un [::id])
                      util/entity-type))
(s/def ::puttable (s/or :map ::entity
                        :operation (s/tuple ::db/operation ::entity)))
(s/def ::puttables (s/coll-of ::puttable))

(defn- refine-qualifiers
  "Removes the namespace for the id key for a entity map and corrects
  missing keyword namespaces.

  The jdbc library doesn't supply the table name as the keyword namespace
  when a CTE is used to create a recursive query. In these cases, we have to
  supply the namespace ourselves."
  [{:keys [include-children? include-parents? entity-type]}]
  (fn [m]
    (let [+ns (if (or include-children? include-parents?)
                           #(keyword (name (or entity-type
                                               (util/entity-type m)))
                                     (name %))
                           identity)]
      (update-keys m (fn [k]
                       (if (= "id" (name k))
                         :id
                         (+ns k)))))))

(def ^:private recursions
  {:account [:parent_id :id]}) ; This is a bit of a kludge, as :parent-id should be translated to snake case, but it's not

; TODO: I think we can manage without doing this directly
; next-jdbc and honeysql both have mechanisms for handling this, I'm pretty sure.
; we just need to set them correctly
(defn- ->snake-case
  [k]
  (keyword (->snake_case_string (namespace k))
           (->snake_case_string (name k))))

(defn- make-query
  [criteria {:as options
             :keys [include-children?
                    include-parents?
                    nil-replacements
                    entity-type]}]
  (-> criteria
      (crt/apply-to types/->sql-ids)
      (crt/apply-to ->sql-refs)
      (criteria->query
        (cond-> (assoc options
                       :quoted? true
                       :column-fn ->snake_case
                       :table-fn ->snake_case
                       :target entity-type)
          nil-replacements (assoc :nil-replacements
                                  (update-keys nil-replacements
                                               ->snake-case))
          include-children? (assoc :recursion (recursions entity-type))
          include-parents? (assoc :recursion (reverse (recursions entity-type)))))))

(defn- after-read*
  ([] (after-read* {}))
  ([{:as options :keys [entity-type]}]
   (comp after-read
         apply-coercions
         ->entity-refs
         (types/qualify-id entity-type)
         (refine-qualifiers options))))

; To save an entity, we need to convert from this:
; {:id <QualifiedID: id=201, entity-type=:entity />
;  :entity/name "Personal"
;  :entity/user {:id <QualifiedID: id=101 entity-type=:user />
;                :user/email "john@doe.com"}}
;
; To this:
; {:entity/id 201
;  :entity/name "Personal"
;  :entity/user-id 101}
;
; The logical steps are:
; 1. Replace all QualifiedID instance with the raw ID value.
; 2. Rename the primary key attribute from :id to :<entity-type>/id
; 3. Replace any references to other entities:
;     a. Extract the value at the :id attribute
;     b. Rename the attribute from :<entity>/<foreign-entity>
;        to :<entity>/<foreign-entity>-id
;
; This is only exposed publicly to support tests that enforce
; short-circuting transaction propagation
(defn put*
  "Executes operations against the database. This function is not entended
  to be used directly."
  [ds entities]
  (let [result (jdbc/with-transaction [tx ds]
                 (->> entities
                      (mapcat deconstruct)
                      (map (comp #(update-in % [1] (comp before-save
                                                         ->sql-refs
                                                         types/->sql-ids))
                                 wrap-oper))
                      (reduce (execute-and-aggregate tx)
                              {:saved []
                               :id-map {}})))]
    (->> (:saved result)
         (map (after-read*))
         (reconstruct))))

(defn- extract-entities
  [ds query options]
  (->> (jdbc/execute! ds query sql-opts)
       (map (after-read* options))
       (post-select options)))

(defn- extract-count
  [ds query]
  (:record-count
    (jdbc/execute-one! ds
                       query
                       sql-opts)))

(defn- select*
  [ds criteria opts]
  (let [options (update-in opts
                           [:entity-type]
                           #(or % (util/entity-type criteria)))
        query (make-query criteria options)
        _ (log/debugf "database select %s with options %s -> %s"
                      (entities/scrub-sensitive-data criteria)
                      options
                      (scrub-values criteria query))]
    (if (:count options)
      (extract-count ds query)
      (extract-entities ds query options))))

(defn- find*
  [ds ^QualifiedID qualified-id]
  {:pre [(instance? QualifiedID qualified-id)]}
  (first
    (select* ds
             {:id qualified-id}
             {:limit 1
              :entity-type (.entity-type qualified-id)})))

(defn- update*
  [ds changes criteria]
  (let [sql (->update (->sql-refs changes)
                      (-> criteria types/->sql-ids ->sql-refs))]
    (log/debugf "database bulk update: change %s for %s -> %s"
                (entities/scrub-sensitive-data changes)
                (entities/scrub-sensitive-data criteria)
                sql)
    (jdbc/execute! ds sql sql-opts)))

(defn- delete*
  [ds entities]
  {:pre [(s/valid? (s/coll-of map?) entities)]}

  (->> entities
       (map #(update-in % [:id] types/unqualify-id))
       (interleave (repeat ::db/delete))
       (partition 2)
       (map vec)
       (put* ds)))

(defn- reset*
  [ds]
  (jdbc/execute! ds ["delete from cached_price; delete from \"user\" cascade"]))

(defn- sql-storage
  [config]
  (let [ds (jdbc/get-datasource config)]
    (reify db/Storage
      (put [_ entities] (put* ds entities))
      (find [_ id] (find* ds id))
      (select [this criteria options] (select* ds criteria (assoc options :storage this)))
      (delete [_ entities] (delete* ds entities))
      (update [_ changes criteria] (update* ds changes criteria))
      (close [_] #_noop)
      (reset [_] (reset* ds)))))

(defmethod db/reify-storage ::db/sql
  [config]
  (db/tracing-storage
    (sql-storage config)
    "sql"))
