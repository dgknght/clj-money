(ns clj-money.models.sql-storage.transactions
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-sort
                                  apply-limit
                                  select-count]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [deep-contains?]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [trx]
  (update-in trx [:transaction-date] t/local-date))

(defmethod stg/select ::models/transaction
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [sql (-> (select :transactions.*)
                (from :transactions)
                (apply-criteria criteria {:target :transaction})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (map after-read (query db-spec sql))))

(defmethod stg/insert ::models/transaction
  [transaction db-spec]
  {:pre [(:transaction-date transaction)]}
  (after-read
    (insert-model db-spec
                  :transactions
                  transaction
                  :entity-id
                  :description
                  :transaction-date
                  :memo
                  :scheduled-transaction-id
                  :value
                  :attachment-count)))

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
                :value
                :attachment-count))

(defmethod stg/delete ::models/transaction
  [{:keys [id transaction-date]} db-spec]
  {:pre [transaction-date]}
  (jdbc/delete! db-spec
                :transactions
                ["id = ? and transaction_date = ?" id transaction-date]))
