(ns clj-money.db.sql
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [camel-snake-kebab.core :refer [->snake_case_keyword
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
            [clj-money.util :as util :refer [temp-id?]]
            [clj-money.db :as db]
            [clj-money.db.sql.queries :refer [criteria->query
                                              ->update]]
            [clj-money.db.sql.types :refer [coerce-id]]))

(defmulti deconstruct (fn [x]
                        (when-not (vector? x)
                          (util/model-type x))))
(defmethod deconstruct :default [m] [m])

(defmulti before-save util/model-type-dispatch)
(defmethod before-save :default [m] m)

(defmulti after-read util/model-type-dispatch)
(defmethod after-read :default [m] m)

(defmulti model-keys util/model-type-dispatch)
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
                  :child? :lot-item/action
                  :children-key :transaction/lot-items}]})

(defn- reconstruct
  [models]
  (if-let [rules (->> models
                   (map util/model-type)
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

(defn- extract-ref-id
  [x]
  (cond
    ; An operation, like [:between start end] or [:in '(1 2 3)]
    (vector? x) (apply vector (first x) (map extract-ref-id (rest x)))

    ; A model or model reference, like {:id 1}
    (map? x)    (if-let [id (:id x)] id x)

    ; A list of values in an :in clause, like [:in #{1 2 3}]
    (coll? x)    (set (map extract-ref-id x))

    ; a value that can be used as-is
    :else x))

(defn- ->sql-refs
  [m]
  (reduce (fn [m k]
            (update-in-if m [k] extract-ref-id))
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
      (util/model-type m1))))
(defmethod post-select :default [_ ms] ms)

(def ^:private infer-table-name
  (comp ->snake_case_keyword
        util/model-type))

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
  [db model]
  (let [table (infer-table-name model)
        s (for-insert table
                      model
                      sql-opts)

        ; TODO: scrub for sensitive data
        _ (log/debugf "database insert %s -> %s" model s)
        result (jdbc/execute-one! db s {:return-keys [:id]})]
    (get-in result [(keyword (name table) "id")])))

(defn- update
  [db model]
  {:pre [(:id model)]}
  (let [table (infer-table-name model)
        s (for-update table
                      (dissoc model :id)
                      {:id (:id model)}
                      sql-opts)

        ; TODO: scrub sensitive data
        _ (log/debugf "database update %s -> %s" model s)
        result (jdbc/execute-one! db s {:return-keys [:id]})]

    (get-in result [(keyword (name table) "id")])))

(defn delete-one
  [ds m]
  (let [s (for-delete (infer-table-name m)
                      {:id (:id m)} ; TODO: find the id attribute
                      sql-opts)]

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
  util/model-type-dispatch)

(defmethod resolve-temp-ids :default
  [model _id-map]
  model)

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

(s/def ::id (some-fn string?
                     integer?
                     uuid?))
(s/def ::model (s/and (s/keys :req-un [::id])
                      util/model-type))
(s/def ::puttable (s/or :map ::model
                       :operation (s/tuple ::db/operation ::model)))
(s/def ::puttables (s/coll-of ::puttable))

; This is only exposed publicly to support tests that enforce
; short-circuting transaction propagation
(defn put*
  "Executes operations against the database. This function is not entended
  to be used directly."
  [ds models]
  {:pre [(s/valid? ::puttables models)]}
  (let [result (jdbc/with-transaction [tx ds]
                 (->> models
                      (mapcat deconstruct)
                      (map (comp #(update-in % [1] (comp before-save
                                                         ->sql-refs))
                                 wrap-oper))
                      (reduce (execute-and-aggregate tx)
                              {:saved []
                               :id-map {}})))]
    (update-in result [:saved] #(->> %
                                     (map (comp after-read
                                                ->model-refs))
                                     (reconstruct)))))

(defn- id-key
  [x]
  (when-let [target (util/model-type x)]
    (keyword (name target) "id")))

(defn- massage-ids
  "Coerces ids and appends the appropriate namespace
  to the :id key"
  [m]
  (let [k (id-key m)]
    (cond-> (crt/apply-to m #(update-in-if % [:id] coerce-id))
      k (rename-keys {:id k}))))

(defn- refine-qualifiers
  "Removes the namespace for the id key for a model map"
  [m]
  (update-keys m (fn [k]
                   (if (= "id" (name k))
                     :id
                     k))))

(def ^:private recursions
  {:account [:parent_id :id]}) ; This is a bit of a kludge, as :parent-id should be translated to snake case, but it's not

(defn- select*
  [ds criteria {:as options
                :keys [include-children?
                       include-parents?]}]
  (let [model-type (util/model-type criteria)
        query (-> criteria
                  (crt/apply-to massage-ids)
                  prepare-criteria
                  (criteria->query (cond-> (assoc options
                                                  :quoted? true
                                                  :target model-type)
                                     include-children? (assoc :recursion (recursions model-type))
                                     include-parents? (assoc :recursion (reverse (recursions model-type))))))]

    ; TODO: scrub sensitive data
    (log/debugf "database select %s with options %s -> %s" criteria options query)

    (if (:count options)
      (jdbc/execute-one! ds
                         query
                         sql-opts)
      (post-select
        (:storage options)
        (map (comp after-read
                   apply-coercions
                   refine-qualifiers
                   ->model-refs)
             (jdbc/execute! ds
                            query
                            sql-opts))))))

(defn- update*
  [ds changes criteria]
  (let [sql (->update (->sql-refs changes)
                      (->sql-refs criteria))]
    (log/debugf "database bulk update: change %s for %s -> %s"
                (pr-str changes)
                (pr-str criteria)
                sql)
    (jdbc/execute! ds sql sql-opts)))

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
  (jdbc/execute! ds ["truncate table cached_price; truncate table \"user\" cascade"]))

(defmethod db/reify-storage ::db/sql
  [config]
  (let [ds (jdbc/get-datasource config)]
    (reify db/Storage
      (put [_ models] (put* ds models))
      (select [this criteria options] (select* ds criteria (assoc options :storage this)))
      (delete [_ models] (delete* ds models))
      (update [_ changes criteria] (update* ds changes criteria))
      (reset [_] (reset* ds)))))
