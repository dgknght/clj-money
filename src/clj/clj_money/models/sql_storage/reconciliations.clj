(ns clj-money.models.sql-storage.reconciliations
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit
                                  apply-sort]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/reconciliation
  [criteria options db-spec]
  (query db-spec (-> (select :reconciliations.*)
                     (from :reconciliations)
                     (apply-criteria criteria {:target :reconciliation})
                     (apply-sort options)
                     (apply-limit options))))

(defmethod stg/insert ::models/reconciliation
  [reconciliation db-spec]
  (insert-model db-spec :reconciliations reconciliation
                :account-id
                :balance
                :end-of-period
                :status))

(defmethod stg/update ::models/reconciliation
  [reconciliation db-spec]
  (update-model db-spec :reconciliations reconciliation
                :account-id
                :balance
                :status
                :end-of-period))

(defmethod stg/delete ::models/reconciliation
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :reconciliations ["id = ?" id]))
