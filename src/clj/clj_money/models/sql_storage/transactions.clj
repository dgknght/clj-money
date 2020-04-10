(ns clj-money.models.sql-storage.transactions
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-sort
                                  select-count]]
            [clj-money.x-platform.util :refer [deep-contains?
                                               deep-get]]
            [clj-money.models :as models]
            [clj-money.partitioning :refer [table-name
                                            with-partitioning]]          
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/transaction
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [result (with-partitioning
                 (partial query db-spec)
                 :transactions
                 (rest (deep-get criteria :transaction-date))
                 options
                 [table]
            (-> (select :transactions.*)
                (from [table :transactions])
                (select-count options)
                (apply-sort options)
                (apply-criteria criteria {:target :transaction})))]
      (if (:count options) ; TODO remove this duplication with select-transaction-items
        (-> result first vals first)
        result)))

(defmethod stg/insert ::models/transaction
  [transaction db-spec]
  (insert-model db-spec
                (table-name (:transaction-date transaction) :transactions)
                transaction
                :entity-id
                :description
                :transaction-date
                :memo
                :value))

(defmethod stg/update ::models/transaction
  [{:keys [transaction-date] :as transaction} db-spec]
  (update-model db-spec
                (table-name transaction-date :transactions)
                transaction
                :description
                :transaction-date
                :memo
                :value))

(defmethod stg/delete ::models/transaction
  [{:keys [id transaction-date]} db-spec]
  (jdbc/delete! db-spec
                (table-name transaction-date :transactions)
                ["id = ?" id]))
