(ns clj-money.models.storage.sql-storage.lot-transactions
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-storage :as stg]
            [clj-money.models.storage.sql-helpers :refer [insert-model
                                                          query
                                                          append-where
                                                          append-limit]]))

(defmethod stg/select ::models/lot-transaction
  [criteria options db-spec]
  (query db-spec (-> (select :lots_transactions.*)
                     (from :lots_transactions)
                     (append-where criteria {:target :lot-transaction})
                     (append-limit options))))

(defmethod stg/insert ::models/lot-transaction
  [lot-transaction db-spec]
  (insert-model db-spec :lots_transactions lot-transaction
                :lot-id
                :transaction-id
                :lot-action
                :shares
                :price
                :action))

(defmethod stg/delete ::models/lot-transaction
  [{:keys [lot-id transaction-id]} db-spec]
  (jdbc/delete! db-spec :lots_transactions [:and
                                            [:= :lot_id lot-id]
                                            [:= :transaction_id transaction-id]]))
