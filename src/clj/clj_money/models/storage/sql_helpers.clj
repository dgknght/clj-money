(ns clj-money.models.storage.sql-helpers
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]
            [camel-snake-kebab.core :refer [->kebab-case
                                            ->snake_case
                                            ->snake_case_string]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-sql-date
                                     to-sql-time
                                     to-local-date]]
            [clj-money.inflection :refer [plural]])
  (:import java.sql.Date
           [org.joda.time LocalDate DateTime]
           [clojure.lang PersistentVector Keyword]))

(extend-protocol jdbc/IResultSetReadColumn
  Date
  (result-set-read-column [v _ _]
    (to-local-date v)))

(extend-protocol jdbc/ISQLValue
  Keyword
  (sql-value [v]
    (name v))

  PersistentVector
  (sql-value [v]
    (into-array v))

  LocalDate
  (sql-value [v]
    (to-sql-date v))

  DateTime
  (sql-value [v]
    (to-sql-time v)))

(defn ->sql-keys
  "Accepts a hash and replaces hyphens in key names
  with underscores"
  [model]
  (transform-keys ->snake_case model))

(defn ->clojure-keys
  "Accepts a hash and replaces underscores in key names
  with hyphens"
  [model]
  (transform-keys ->kebab-case model))

(defn insert-model
  "Inserts a record into the specified table"
  [db-spec table model & allowed-keys]
  (->> (select-keys model allowed-keys)
       ->sql-keys
       (jdbc/insert! db-spec table)
       first
       ->clojure-keys))

(defn update-model
  "Updates a record in the specified table"
  [db-spec table {:keys [id] :as model} & options]
  {:pre [id]}

  (let [[criteria allowed-keys] (if (keyword? (first options))
                                  [[:= :id id] options]
                                  [(first options) (rest options)])
        sql (sql/format (-> (h/update table)
                            (h/sset (-> model
                                        (select-keys allowed-keys)
                                        (assoc :updated-at (t/now))
                                        ->sql-keys))
                            (h/where criteria)))]
    (log/debug "update-model" (prn-str model) "in" table ": " (prn-str sql))
    (jdbc/execute! db-spec sql)))

(defn append-sort
  [sql options]
  (if-let [s (:sort options)]
    (apply h/order-by sql s)
    sql))

(defn append-paging
  [sql options]
  (cond-> sql
    (:per-page options) (h/limit (:per-page options))
    (:page options) (h/offset (* (:per-page options) (- (:page options) 1)))))

(defn append-limit
  [sql options]
  (if-let [limit (:limit options)]
    (h/limit sql limit)
    sql))

(defn- ensure-not-keyword
  "Make sure the value is not a keyword. It could be a string, an integer
  or anything else."
  [value]
  (if (keyword? value)
    (name value)
    value))

(defn- map-entry->statements
  [[k v]]
  (if (coll? v)
    (case (first v)

      (:= :> :>= :<= :< :<> :!=)
      [[(first v) k (ensure-not-keyword (second v))]]

      :between
      [[:>= k (ensure-not-keyword (second v))]
       [:<= k (ensure-not-keyword (nth v 2))]]

      [[:in k (map ensure-not-keyword v)]])
    [[:= k (ensure-not-keyword v)]]))

(def ^:private relationships
  {#{:transaction :lot-transaction}  {:primary-table :transactions
                                      :foreign-table :lots_transactions
                                      :foreign-id    :transaction_id}
   #{:transaction :transaction-item} {:primary-table :transactions
                                      :foreign-table :transaction_items
                                      :foreign-id    :transaction_id}
   #{:entity :transaction}           {:primary-table :entities
                                      :foreign-table :transactions
                                      :foreign-id    :entity_id}
   #{:entity :account}               {:primary-table :entities
                                      :foreign-table :accounts
                                      :foreign-id    :entity_id}
   #{:lot :commodity}                {:primary-table :commodities
                                      :foreign-table :lots
                                      :foreign-id    :commodity_id}
   #{:price :commodity}              {:primary-table :commodities
                                      :foreign-table :prices
                                      :foreign-id    :commodity_id}
   #{:commodity :entity}             {:primary-table :entities
                                      :foreign-table :commodities
                                      :foreign-id    :entity_id}})

(defn- relationship
  [rel-key]
  (merge {:primary-id :id}
         (get-in relationships [(set rel-key)])))

(defn- col-ref
  [table column]
  (keyword (->> [table column]
                (map ensure-not-keyword)
                (string/join "."))))

(def ^:private table-names
  {:lot-transaction :lots_transactions})

(defn- model->table
  [m]
  (get-in table-names [m] (-> m name plural keyword)))

(defn- resolve-join-col
  "Replaces a column spec that references another table with
  a property table.column expression.

  (resolve-join-col [:lot-transaction :lot-id]) => :lots_transactions.lot_id
  {[:lot-transaction :lot-id] (:id lot)}
  [:= :lots_transactions.lot_id (:id lot)]"
  [column-spec _options]
  (if (coll? column-spec)
    (let [model (->> column-spec
                     reverse
                     (drop 1)
                     first)]
      (col-ref (model->table model)
               (->snake_case_string (last column-spec))))
    column-spec))

(defmulti ->where
  (fn [criteria & _]
    (if (sequential? criteria)
      :clause
      :map)))

(defmethod ->where :map
  [m {:keys [prefix] :as options}]
  (let [prefix-fn (if prefix
                    (fn [k]
                      (if (string/includes? (name k) ".")
                        k
                        (keyword
                          (format "%s.%s"
                                  (ensure-not-keyword prefix)
                                  (name k)))))
                    identity)
        result (->> m
                    (map (fn [kv]
                           (update-in kv [0] (comp prefix-fn
                                                   #(resolve-join-col % options)))))
                    (mapcat map-entry->statements))]
    (if (= 1 (count result))
      (first result)
      (concat [:and]
              result))))

(defmethod ->where :clause
  [m options]
  (concat [(first m)]
          (map #(->where % options)
               (rest m))))

(defn- apply-criteria-join
  [sql rel-key {:keys [target-alias]}]
  (let [existing-joins (->> (get-in sql [:join])
                            (partition 2)
                            (map (comp #(if (vector? %)
                                          (second %)
                                          %)
                                       first))
                            set)
        new-table (model->table (second rel-key))
        {:keys [primary-table
                primary-id
                foreign-table
                foreign-id]} (relationship rel-key)]
    (assert primary-table (str "No relationship defined for " (prn-str rel-key)))
    (if (existing-joins new-table)
      sql
      (h/merge-join sql new-table
                    [:=
                     (col-ref (or target-alias primary-table) ; TODO: this will cause a problem with an alias specified and depth > 1
                              primary-id)
                     (col-ref foreign-table foreign-id)]))))

(defn- apply-criteria-join-chain
  [sql join-key {:keys [target] :as options}]
  (->> (butlast join-key)
       (concat [target])
       (partition 2 1)
       (reduce #(apply-criteria-join %1 %2 options)
               sql)))

(defn- apply-criteria-joins
  "Creates join clauses for criteria with keys that reference other tables.

  {[:lot-transaction :lot-id] (:id lot)}
  (join sql :lots_transactions [:= :transactions.id :lots_transactions.transaction_id])"
  [sql join-keys options]
  {:pre [(:target options)]}

  (reduce #(apply-criteria-join-chain %1 %2 options)
          sql
          join-keys))

(defmulti ^:private extract-join-keys
  #(if (sequential? %)
     :clause
     :map))

(defmethod ^:private extract-join-keys :clause
  [criteria]
  (mapcat #(extract-join-keys %)
          (rest criteria)))

(defmethod ^:private extract-join-keys :map
  [criteria]
  (->> (keys criteria)
       (filter coll?)))

(defn- ensure-criteria-joins
  [sql criteria options]
  (let [join-keys (extract-join-keys criteria)]
    (if (seq join-keys)
      (apply-criteria-joins sql join-keys options)
      sql)))

(defn append-where
  ([sql criteria]
   (append-where sql criteria {}))
  ([sql criteria options]
   (if (seq criteria)
     (-> sql
         (h/merge-where (->where criteria options))
         (ensure-criteria-joins criteria options))
     sql)))

(defn select-count
  "If the specified options contains :count true, replace the
  select clause with count(*)"
  [sql options]
  (if (:count options)
    (h/select sql :%count.*)
    sql))

(defn query
  "Executes a SQL query and maps field names into
  clojure keys"
  [db-spec sql]
  {:pre [(:select sql)]}
  (let [formatted (sql/format sql)]
    (log/debug "query" (prn-str formatted))
    (map ->clojure-keys (jdbc/query db-spec formatted))))
