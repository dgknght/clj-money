(ns clj-money.models.sql-storage.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/budget-item
  [criteria options db-spec]
  (query db-spec (-> (select :*)
                     (from :budget_items)
                     (apply-criteria criteria)
                     (apply-limit options))))

(defmethod stg/insert ::models/budget-item
  [budget-item db-spec]
  (insert-model db-spec :budget_items budget-item
                :budget-id
                :account-id
                :periods))

(defmethod stg/update ::models/budget-item
  [budget-item db-spec]
  (update-model db-spec :budget_items budget-item
                :account-id
                :periods))

(defmethod stg/delete ::models/budget-item
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :budget_items ["id = ?" id]))
