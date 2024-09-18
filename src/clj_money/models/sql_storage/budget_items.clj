(ns clj-money.models.sql-storage.budget-items
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
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
                :spec
                :periods))

(defmethod stg/update ::models/budget-item
  [budget-item db-spec]
  (let [sql ["UPDATE budget_items SET account_id = ?, periods = ?, spec = ? WHERE id = ?"
             (:account-id budget-item)
             (:periods budget-item)
             (:spec budget-item)
             (:id budget-item)]]
    (log/debugf "update budget item: %s" (prn-str sql))
    (jdbc/execute! db-spec sql)))

(defmethod stg/delete ::models/budget-item
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :budget_items ["id = ?" id]))
