(ns clj-money.db.sql
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [camel-snake-kebab.core :refer [->snake_case_keyword]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql.builder :refer [for-insert
                                           for-update
                                           for-delete]]
            [next.jdbc.date-time]
            [stowaway.criteria :as crt]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.inflection :refer [plural
                                                singular]]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.util :as util :refer [temp-id temp-id?]]
            [clj-money.db :as db]
            [clj-money.db.sql.queries :refer [criteria->query
                                              ->update]]
            [clj-money.db.sql.types :refer [coerce-id]]))

(defmulti deconstruct (fn [x]
                        (when-not (vector? x)
                          (db/model-type x))))
(defmethod deconstruct :default [m] [m])

(defmulti before-save db/type-dispatch)
(defmethod before-save :default [m] m)

(defmulti after-read db/model-type)
(defmethod after-read :default [m] m)

(defmulti model-keys db/model-type)
(defmethod model-keys :default [_] [])

(def ^:private model-ref-keys
  [:image/user
   :import/user
   :identity/user
   :entity/user
   :grant/entity
   :grant/user
   :commodity/entity
   :price/commodity
   :account/entity
   :account/commodity
   :account/parent
   :transaction/entity
   :transaction/scheduled-transaction
   :transaction-item/account
   :transaction-item/reconciliation
   :transaction-item/transaction
   :lot-item/transaction
   :lot-item/lot
   :scheduled-transaction/entity
   :scheduled-transaction-item/account
   :reconciliation/account
   :budget/entity
   :budget-item/account
   :budget-item/budget
   :attachment/image
   :attachment/transaction
   :lot/account
   :lot/commodity
   :lot-transaction/transaction])

(def ^:private reconstruction-rules
  {:budget [{:parent? :budget/name
             :child? :budget-item/account
             :children-key :budget/items}]
   :reconciliation [{:parent? :reconciliation/end-of-period
                     :child? :transaction-item/reconciliation
                     :children-key :reconciliation/item-refs}]
   :scheduled-transaction [{:parent? :scheduled-transaction/description
                            :child? :scheduled-transaction-item/action
                            :children-key :scheduled-transaction/items}]
   :transaction [{:parent? :transaction/description
                  :child? :transaction-item/action
                  :children-key :transaction/items}
                 {:parent? :transaction/description
                  :child? :lot-item/lot-action
                  :children-key :transaction/lot-items}]})

(defn- reconstruct
  [models]
  (if-let [rules (->> models
                   (map db/model-type)
                   set
                   (mapcat reconstruction-rules)
                   (filter identity)
                   seq)]
    (reduce (fn [ms rule-map]
              (util/reconstruct rule-map ms))
            models
            rules)
    models))

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
        model-ref-keys))

(def ^:private model->sql-ref-map
  (zipmap model-ref-keys sql-ref-keys))

(defn- ->sql-refs
  [m]
  (reduce (fn [m k]
            (update-in-if m [k] (comp coerce-id ->id)))
          (rename-keys m model->sql-ref-map)
          sql-ref-keys))

(def ^:private sql->model-ref-map
  (zipmap sql-ref-keys model-ref-keys))

(defn- ->model-refs
  [m]
  (reduce (fn [m k]
            (update-in-if m [k] util/->model-ref))
          (rename-keys m sql->model-ref-map)
          model-ref-keys))

; convert keywords to strings (or can this be done with SettableParameter?)
; {:account/type :asset} -> {:account/type "asset"}
; maybe convert java-time to sql? I think next-jdbc.date-time already handles this
; {:transaction/transaction-date (t/local-date 2020 1 1)} -> {:transaction/transaction-date "2020-01-01"}
(defn- prepare-criteria
  [criteria]
  (crt/apply-to criteria ->sql-refs))

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

        ; TODO: scrub for sensitive data
        _ (log/debugf "database insert %s -> %s" model s)
        result (jdbc/execute-one! db s {:return-keys [:id]})]
    (get-in result [(keyword (name table) "id")])))

(defn- update
  [db model]
  {:pre [(:id model)]}

  (when (not (db/model-type model))
    (throw (ex-info "Unable to identify the model type" {:model model})))

  (let [table (infer-table-name model)
        s (for-update table
                      (dissoc model :id)
                      {:id (:id model)}
                      jdbc/snake-kebab-opts)

        ; TODO: scrub sensitive data
        _ (log/debugf "database update %s -> %s" model s)
        result (jdbc/execute-one! db s {:return-keys [:id]})]

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

(defn- ensure-id
  "When saving a new record, make sure we have a temp id"
  [m]
  (if (map? m) ; this could also be a vector like [:delete {:id 123}]
    (update-in m [:id] (fn [id]
                         (or id (temp-id))))
    m))

(defn- strip-unrecognized-keys
  [m]
  (if-let [keys (seq (model-keys m))]
    (select-keys m keys)
    m))

(defn- execute-and-aggregate
  "Returns a function that executes the database operation, saves the result
  and updates the id map for resolving temporary ids"
  [ds]
  (fn [{:as result :keys [id-map]} [operator m]]
    (let [id-resolved (cond-> (strip-unrecognized-keys m)
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

  (when-let [m (some (complement db/model-type) models)]
    (pprint {::put* m
             ::models models})
    (throw (ex-info "All models must have a type" {:model m})))

  (jdbc/with-transaction [tx ds]
    (->> models
         (map ensure-id)
         (mapcat deconstruct)
         (map (comp #(update-in % [1] (comp before-save
                                            ->sql-refs))
                    wrap-oper))
         (reduce (execute-and-aggregate tx)
                 {:saved []
                  :id-map {}})
         :saved
         (map (comp after-read
                    ->model-refs))
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
  "Returns a function that takes a map and singularizes qualifiers based on
  plural table names and strips the qualifier from the :id attribute. If no
  qualifier is present, use the specified model type"
  [model-type]
  (let [qual (name model-type)]
    (fn [m]
      (update-keys m #(let [q (namespace %)
                            k (name %)]
                        (if (= "id" k)
                          :id
                          (if q
                            (keyword (singular q) k)
                            (keyword qual k))))))))

(def ^:private recursions
  {:account [:parent-id :id]})

(defn- select*
  [ds criteria {:as options
                :keys [include-children?
                       include-parents?]}]
  (let [model-type (db/model-type criteria)
        query (-> criteria
                  (crt/apply-to massage-ids)
                  prepare-criteria
                  (criteria->query (cond-> (assoc options :target model-type)
                                     include-children? (assoc :recursion (recursions model-type))
                                     include-parents? (assoc :recursion (reverse (recursions model-type))))))]

    ; TODO: scrub sensitive data
    (log/debugf "database select %s with options %s -> %s" criteria options query)

    (if (:count options)
      (jdbc/execute-one! ds
                         query
                         jdbc/unqualified-snake-kebab-opts)
      (post-select
        (:storage options)
        (map (comp after-read
                   apply-coercions
                   ->model-refs
                   (refine-qualifiers model-type))
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
