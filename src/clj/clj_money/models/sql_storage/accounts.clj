(ns clj-money.models.sql-storage.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [honeysql.helpers :refer [with-recursive
                                      select
                                      from
                                      join]]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(def ^:private fields
  [:accounts.id
   :accounts.entity_id
   :accounts.type
   :accounts.name
   :accounts.parent_id
   :accounts.quantity
   :accounts.tags
   :accounts.value
   :accounts.hidden
   :accounts.earliest_transaction_date
   :accounts.latest_transaction_date
   :accounts.created_at
   :accounts.updated_at])

(defn- select-sql-with-downward-recursion
  [criteria options]
  (-> (with-recursive [:raccounts
                       {:union [(-> (apply select fields)
                                    (from :accounts)
                                    (apply-criteria criteria (merge options
                                                                    {:target :account}))
                                    (apply-limit options))
                                (-> (apply select fields)
                                    (from :accounts)
                                    (join [:raccounts :p] [:= :p.id :accounts.parent_id]))]}])
      (select :*)
      (from :raccounts)))

(defn- select-sql
  [criteria options]
  (-> (select :accounts.*)
      (from :accounts)
      (apply-criteria criteria (merge options
                                      {:target :account
                                       :prefix :accounts}))
      (apply-limit options)))

(defmethod stg/select ::models/account
  [criteria {:keys [include-children?] :as options} db-spec]
  (let [sql (if include-children?
              (select-sql-with-downward-recursion criteria options)
              (select-sql criteria options))]
    (log/debugf "select %s" criteria)
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
