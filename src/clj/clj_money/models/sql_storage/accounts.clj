(ns clj-money.models.sql-storage.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [honeysql.helpers :refer [with-recursive
                                      select
                                      from
                                      join]]
            [stowaway.sql :refer [apply-limit]]
            [dgknght.app-lib.core :refer [update-in-if]]
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
   :accounts.allocations
   :accounts.quantity
   :accounts.system_tags
   :accounts.user_tags
   :accounts.value
   :accounts.price_as_of
   :accounts.hidden
   :accounts.earliest_transaction_date
   :accounts.latest_transaction_date
   :accounts.created_at
   :accounts.updated_at])

(defn- after-read
  [account]
  (-> account
      (update-in-if [:earliest-transaction-date] t/local-date)
      (update-in-if [:latest-transaction-date] t/local-date)))

(defn- select-sql-with-downward-recursion
  [criteria options]
  (map after-read
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
           (from :raccounts))))

(defn- select-sql
  [criteria options]
  (-> (select :accounts.*
              [:commodities.type :commodity_type]
              [:commodities.symbol :commodity_symbol]
              [:commodities.name :commodity_name])
      (from :accounts)
      (join :commodities [:= :commodities.id :accounts.commodity_id])
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
  (after-read
    (insert-model db-spec :accounts account
                  :name
                  :type
                  :system-tags
                  :user-tags
                  :commodity-id
                  :entity-id
                  :parent-id
                  :allocations
                  :quantity
                  :value
                  :price-as-of
                  :earliest-transaction-date
                  :latest-transaction-date)))

(defmethod stg/update ::models/account
  [account db-spec]
  (update-model db-spec :accounts account
                :name
                :type
                :system-tags
                :user-tags
                :commodity-id
                :parent-id
                :allocations
                :quantity
                :value
                :price-as-of
                :earliest-transaction-date
                :latest-transaction-date))

(defmethod stg/delete ::models/account
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :accounts ["id = ?" id]))
