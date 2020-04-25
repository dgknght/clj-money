(ns clj-money.models.storage.sql-helpers
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [camel-snake-kebab.core :refer [->kebab-case
                                            ->snake_case]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-sql-date
                                     to-sql-time
                                     to-local-date]]
            [stowaway.sql :as storage])
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
   #{:entity :budget}                {:primary-table :entities
                                      :foreign-table :budgets
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

(def ^:private table-names
  {:lot-transaction :lots_transactions})

(defn apply-criteria
  ([sql criteria]
   (apply-criteria sql criteria {}))
  ([sql criteria options]
   (storage/apply-criteria sql criteria (merge options
                                               {:table-names table-names
                                                :relationships relationships}))))

(defn query
  "Executes a SQL query and maps field names into
  clojure keys"
  [db-spec sql]
  {:pre [(:select sql)]}
  (let [formatted (sql/format sql)]
    (log/debug "query" (prn-str formatted))
    (map ->clojure-keys (jdbc/query db-spec formatted))))
