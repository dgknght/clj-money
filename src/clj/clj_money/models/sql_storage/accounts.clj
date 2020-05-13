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

(defmethod stg/select ::models/account
  [criteria options db-spec]
  (let [sql (-> (select :accounts.*)
                (from :accounts)
                (apply-criteria criteria {:target :account
                                        :prefix :accounts})
                (apply-limit options))]
    (query db-spec sql)))

(defmethod stg/insert ::models/account
  [account db-spec]
  (insert-model db-spec :accounts account
                :name
                :type
                :tags
                :commodity-id
                :entity-id
                :parent-id
                :quantity
                :value
                :earliest-transaction-date
                :latest-transaction-date))

(defmethod stg/update ::models/account
  [account db-spec]
  (update-model db-spec :accounts account
                :name
                :type
                :tags
                :commodity-id
                :parent-id
                :quantity,
                :value
                :earliest-transaction-date
                :latest-transaction-date))

(defmethod stg/delete ::models/account
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :accounts ["id = ?" id]))
