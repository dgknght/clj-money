(ns clj-money.models.sql-storage.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clj-time.core :as t]
            [honeysql.helpers :refer [select
                                      update
                                      sset
                                      from
                                      join
                                      left-join]]
            [honeysql.format :as sql]
            [stowaway.sql :refer [apply-sort
                                  apply-limit
                                  select-count]]
            [clj-money.x-platform.util :refer [deep-contains?]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          ->sql-keys
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/transaction-item
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [sql (-> (select :transaction_items.*
                        :transactions.description
                        [:reconciliations.status :reconciliation_status])
                (from :transaction_items)
                (join :transactions [:= :transactions.id :transaction_items.transaction_id])
                (left-join :reconciliations [:= :reconciliations.id :transaction_items.reconciliation_id])
                (apply-criteria criteria {:target :transaction-item})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (query db-spec sql)))

(defmethod stg/insert ::models/transaction-item
  [item db-spec]
  {:pre [(:transaction-date item)]}
  (insert-model db-spec
                  :transaction_items
                  item
                  :transaction-id
                  :transaction-date
                  :account-id
                  :action
                  :quantity
                  :negative
                  :value
                  :index
                  :balance
                  :memo))

(defn- update-item
  [item db-spec]
  {:pre [(:transaction-date item)]}
  (update-model db-spec
                :transaction_items
                item
                :transaction-date
                :quantity
                :value
                :negative
                :memo
                :action
                :index
                :balance
                :account-id))

(def ^:private allowed-keys
  #{:transaction-date
    :account-id
    :action
    :quantity
    :value
    :balance
    :memo
    :index
    :reconciliation-id
    :negative})

(defn- update-items
  [attr criteria db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [sql (-> (update :transaction_items)
                (sset (-> attr
                          (select-keys allowed-keys)
                          (assoc :updated-at (t/now))
                          ->sql-keys))
                (apply-criteria criteria)
                sql/format)]
    (log/debugf "update transaction_items %s %s -> %s" attr criteria sql)
    (first (jdbc/execute! db-spec sql))))

(defmethod stg/update ::models/transaction-item
  [& args]
  (case (count args)
    2 (apply update-item args)
    3 (apply update-items args)
    (throw (.ArityException "Expected 2 or 3 arguments"))))

(defmethod stg/delete ::models/transaction-item
  [{:keys [transaction-date id]} db-spec]
  {:pre [transaction-date]}
  (jdbc/delete! db-spec
                :transaction_items
                ["id = ? and transaction_date = ?" id transaction-date]))
