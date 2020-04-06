(ns clj-money.models.storage.sql-storage.lots
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-sort
                                  apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/lot
  [criteria options db-spec]
  (query db-spec (-> (select :*)
                     (from :lots)
                     (apply-criteria criteria {:target :lot})
                     (apply-limit options)
                     (apply-sort options))))

(defmethod stg/insert ::models/lot
  [lot db-spec]
  (insert-model db-spec :lots lot
                :commodity-id
                :account-id
                :purchase-price
                :purchase-date
                :shares-purchased
                :shares-owned))

(defmethod stg/update ::models/lot
  [lot db-spec]
  (update-model db-spec :lots lot
                :purchase-date
                :account-id
                :commodity-id
                :purchase-price
                :shares-owned
                :shares-purchased))

(defmethod stg/delete ::models/lot
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :lots ["id = ?" id]))
