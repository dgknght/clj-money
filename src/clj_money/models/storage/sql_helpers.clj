(ns clj-money.models.storage.sql-helpers
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [cheshire.core :as json]
            [clj-money.json]
            [camel-snake-kebab.core :refer [->snake_case_string
                                            ->kebab-case-keyword]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [java-time.api :as t]
            [stowaway.sql :as storage]
            [dgknght.app-lib.core :refer [update-in-if]])
  (:import [java.sql PreparedStatement]
           java.util.Date
           org.postgresql.util.PGobject
           [clojure.lang PersistentArrayMap PersistentVector Keyword]))

(defn- ->json
  ([value]
   (doto (PGobject.)
     (.setValue (json/generate-string value))))
  ([value type-name]
   (doto (PGobject.)
     (.setType type-name)
     (.setValue (json/generate-string value)))))

(extend-protocol jdbc/ISQLValue
  Keyword
  (sql-value [v]
    (name v))

  PersistentArrayMap
  (sql-value [v]
    (->json v))

  PersistentVector
  (sql-value [v]
    (into-array v)))

(extend-protocol jdbc/ISQLParameter
  PersistentArrayMap
  (set-parameter [value ^PreparedStatement stmt ^long index]
    (if-let [type-name (.getParameterTypeName
                         (.getParameterMetaData stmt)
                         index)]
      (.setObject stmt index (->json value type-name))
      (.setObject stmt index value))))

(extend-protocol jdbc/IResultSetReadColumn
  Date
  (result-set-read-column [value _ _]
    (t/local-date-time value)))

(defn insert-model
  "Inserts a record into the specified table"
  [db-spec table model & allowed-keys]
  (jdbc/insert! db-spec
                table
                (select-keys model allowed-keys)
                {:entities ->snake_case_string
                 :identifiers ->kebab-case-keyword
                 :result-set-fn first}))

(defn- redact-sensitive
  [model]
  (reduce #(update-in-if %1 [%2] (constantly "****"))
          model
          [:password]))

(defn update-model
  "Updates a record in the specified table"
  [db-spec table {:keys [id] :as model} & options]
  {:pre [id]}
  (let [[criteria allowed-keys] (if (keyword? (first options))
                                  [[:= :id id] options]
                                  [(first options) (rest options)])
        updated-model (-> model
                          (select-keys allowed-keys)
                          (assoc :updated-at (t/sql-timestamp)))
        where-clause (update-in (sql/format (h/where criteria))
                                [0]
                                string/replace
                                #"^WHERE "
                                "")]
    (log/debugf "update model %s where %s"
                (redact-sensitive updated-model)
                where-clause)
    (jdbc/update! db-spec table
                  updated-model
                  where-clause
                  {:entities ->snake_case_string})))

(def ^:private relationships
  {#{:transaction :lot-transaction}     {:primary-table :transactions
                                         :primary-id    [:transaction_date :id]
                                         :foreign-table :lots_transactions
                                         :foreign-id    [:transaction_date :transaction_id]}
   #{:transaction :transaction-item}    {:primary-table :transactions
                                         :primary-id    [:transaction_date :id]
                                         :foreign-table :transaction_items
                                         :foreign-id    [:transaction_date :transaction_id]}
   #{:account :transaction-item}        {:primary-table :accounts
                                         :foreign-table :transaction_items
                                         :foreign-id    :account_id}
   #{:entity :transaction}              {:primary-table :entities
                                         :foreign-table :transactions
                                         :foreign-id    :entity_id}
   #{:entity :scheduled-transaction}    {:primary-table :entities
                                         :foreign-table :scheduled_transactions
                                         :foreign-id    :entity_id}
   #{:attachment :transaction}          {:primary-table :transactions
                                         :primary-id    [:transaction_date :id]
                                         :foreign-table :attachments
                                         :foreign-id    [:transaction_date :transaction_id]}
   #{:reconciliation :account}          {:primary-table :accounts
                                         :foreign-table :reconciliations
                                         :foreign-id    :account-id}
   #{:reconciliation :transaction-item} {:primary-table :reconciliations
                                         :primary-id    [:end_of_period :id]
                                         :foreign-table :transaction_items
                                         :foreign-id    [:reconciliation_end_of_period :reconciliation_id]}
   #{:entity :account}                  {:primary-table :entities
                                         :foreign-table :accounts
                                         :foreign-id    :entity_id}
   #{:entity :budget}                   {:primary-table :entities
                                         :foreign-table :budgets
                                         :foreign-id    :entity_id}
   #{:lot :commodity}                   {:primary-table :commodities
                                         :foreign-table :lots
                                         :foreign-id    :commodity_id}
   #{:lot :lot-transaction}             {:primary-table :lots
                                         :foreign-table :lots_transactions
                                         :foreign-id    :lot_id}
   #{:price :commodity}                 {:primary-table :commodities
                                         :foreign-table :prices
                                         :foreign-id    :commodity_id}
   #{:commodity :entity}                {:primary-table :entities
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
    (try
      (jdbc/query db-spec formatted {:identifiers ->kebab-case-keyword})
      (catch Exception e
        (log/errorf e "Unable to complete query: %s" formatted)
        (throw (Exception. "Unable to complete the query" e))))))
