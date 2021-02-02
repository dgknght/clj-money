(ns clj-money.models.sql-storage.transactions
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-sort
                                  apply-limit
                                  select-count]]
            [clj-money.util :refer [deep-contains?]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/transaction
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [sql (-> (select :transactions.*)
                (from :transactions)
                (apply-criteria criteria {:target :transaction})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (query db-spec sql)))

(defmethod stg/insert ::models/transaction
  [transaction db-spec]
  {:pre [(:transaction-date transaction)]}
  (insert-model db-spec
                :transactions
                transaction
                :entity-id
                :description
                :transaction-date
                :memo
                :scheduled-transaction-id
                :value))

(defmethod stg/update ::models/transaction
  [transaction db-spec]
  {:pre [(:transaction-date transaction)]}
  (update-model db-spec
                :transactions
                transaction
                :description
                :transaction-date
                :memo
                :scheduled-transaction-id
                :value))

(defmethod stg/delete ::models/transaction
  [{:keys [id transaction-date]} db-spec]
  {:pre [transaction-date]}
  (jdbc/delete! db-spec
                :transactions
                ["id = ? and transaction_date = ?" id transaction-date]))
