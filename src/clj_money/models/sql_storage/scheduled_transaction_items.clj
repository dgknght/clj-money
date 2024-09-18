(ns clj-money.models.sql-storage.scheduled-transaction-items
  (:require [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit
                                  apply-sort]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/insert ::models/scheduled-transaction-item
  [item db-spec]
  (insert-model db-spec :scheduled_transaction_items item
                :scheduled-transaction-id
                :action
                :account-id
                :quantity
                :memo))

(defmethod stg/select ::models/scheduled-transaction-item
  [criteria options db-spec]
  (query db-spec (-> (select :scheduled_transaction_items.*)
                     (from :scheduled_transaction_items)
                     (apply-criteria criteria)
                     (apply-limit options)
                     (apply-sort options))))

(defmethod stg/update ::models/scheduled-transaction-item
  [item db-spec]
  (update-model db-spec
                :scheduled_transaction_items
                item 
                :action
                :quantity
                :account-id
                :memo))
