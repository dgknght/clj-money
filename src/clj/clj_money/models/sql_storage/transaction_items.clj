(ns clj-money.models.sql-storage.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      update
                                      sset
                                      from
                                      join
                                      where
                                      left-join]]
            [honeysql.format :as sql]
            [stowaway.sql :refer [apply-sort
                                  apply-limit
                                  select-count
                                  map->where]]
            [clj-money.x-platform.util :refer [deep-contains?
                                               deep-get]]
            [clj-money.models :as models]
            [clj-money.partitioning :refer [table-name
                                            with-partitioning]]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          ->sql-keys
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/transaction-item
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [opts (if (:count options)
               options
               (merge
                 {:sort [[:transaction_items.index :desc]]}
                 options))
        result (with-partitioning
                 (partial query db-spec)
                 [:transaction_items :transactions]
                 (rest (deep-get criteria :transaction-date))
                 opts
                 [tables]
                 (-> (select :transaction_items.* :transactions.description, [:reconciliations.status :reconciliation_status])
                     (from [(first tables) :transaction_items])
                     (join [(second tables) :transactions]
                           [:= :transactions.id :transaction_items.transaction_id])
                     (left-join :reconciliations [:= :reconciliations.id :transaction_items.reconciliation_id])
                     (select-count opts)
                     (apply-criteria criteria
                                   {:prefix "transaction_items"
                                    :target :transaction-item})
                     (apply-sort opts)
                     (apply-limit opts)))]
    (if (:count opts)
      (-> result first vals first)
      result)))

(defmethod stg/insert ::models/transaction-item
  [{:keys [transaction-date] :as item} db-spec]
  (insert-model db-spec
                  (table-name transaction-date :transaction_items)
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
  [{:keys [transaction-date] :as item} db-spec]
      (update-model db-spec
                    (table-name transaction-date :transaction_items)
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

(defn- update-items
 [attr criteria db-spec] 
 {:pre [(deep-contains? criteria :transaction-date)]}
 (let [transaction-date (deep-get criteria :transaction-date)
       date-range (if (sequential? transaction-date)
                    transaction-date
                    [transaction-date])]
   (->> date-range ; TODO: add this to partitions to get one query per table
        (map #(-> (update (table-name % :transaction_items))
                  (sset (->sql-keys attr))
                  (where (map->where criteria {}))
                  sql/format))
        (reduce (fn [update-count sql]
                  (log/debug "update transaction items " (prn-str attr) " matching " (prn-str criteria) ": " (prn-str sql))
                  (+ update-count (first (jdbc/execute! db-spec sql))))
                0))))

(defmethod stg/update ::models/transaction-item
  [& args]
  (case (count args)
    2 (apply update-item args)
    3 (apply update-items args)
    (throw (.ArityException "Expected 2 or 3 arguments"))))

(defmethod stg/delete ::models/transaction-item
  [{:keys [transaction-date id]} db-spec]
  (jdbc/delete! db-spec
                (table-name transaction-date :transaction_items)
                ["id = ?" id]))
