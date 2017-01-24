(ns clj-money.models.storage.sql-storage
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-time.jdbc]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-money.models.storage :refer [Storage]])
  (:import java.sql.BatchUpdateException))

(defn- exists?
  [db-spec table where]
  (let [sql (sql/format (-> (h/select [:%count.id :record_count])
                              (h/from table)
                              (h/where where)))]
      (= 1 (->> sql
           (jdbc/query db-spec)
           first
           :record_count))))

(defn- update-keys
  [model f]
  (when model
    (->> model
         (map #(update-in % [0] f))
         (into {}))))

(defn- ->sql-key
  "Accepts a keyword returns it with hyphens replaced with underscores"
  [k]
  (keyword (string/replace (name k) "-" "_")))

(defn- ->sql-keys
  "Accepts a hash and replaces hyphens in key names
  with underscores"
  [model]
  (update-keys model ->sql-key))

(defn- ->clojure-key
  "Accepts a keyword returns it with underscores replaced with hyphens"
  [k]
  (keyword (string/replace (name k) "_" "-")))

(defn- ->clojure-keys
  "Accepts a hash and replaces underscores in key names
  with hyphens"
  [model]
  (if (map? model)
    (update-keys model ->clojure-key)
    model))

(defn- insert
  "Inserts a record into the specified table"
  [db-spec table model & allowed-keys]
  (->> (select-keys model allowed-keys)
       ->sql-keys
       (jdbc/insert! db-spec table)
       first
       ->clojure-keys))

(defn- ->update-set
  "Prepares a model for update"
  [model & keys]
  (-> model
      (select-keys keys)
      (assoc :updated-at (t/now))
      ->sql-keys))

(defn- append-paging
  [sql options]
  (cond-> sql
    (:per-page options) (h/limit (:per-page options))
    (:page options) (h/offset (* (:per-page options) (:page options)))))

(defn- query
  "Executes a SQL query and maps field names into
  clojure keys"
  [db-spec sql-map]
  (->> (sql/format sql-map)
       (jdbc/query db-spec)
       (map ->clojure-keys)))

(defn- query-scalar
  "Executes the SQL query and returns the first column of
  the first record of the result"
  [db-spec sql-map]
  (->> (sql/format sql-map)
       (jdbc/query db-spec)
       first
       vals
       first))

(defn- transaction-item-base-query
  []
  (-> (h/select :i.* :t.transaction_date :t.description, [:r.status "reconciliation_status"])
      (h/from [:transaction_items :i])
      (h/join [:transactions :t] [:= :t.id :i.transaction_id])
      (h/left-join [:reconciliations :r] [:= :r.id :i.reconciliation_id])))

(deftype SqlStorage [db-spec]
  Storage

  ; Users
  (create-user
    [_ user]
    (insert db-spec :users user :first-name
                                :last-name
                                :email
                                :password))

  (select-users
    [_]
    (query db-spec (-> (h/select :first_name :last_name :email)
                       (h/from :users))))

  (find-user-by-email
    [this email]
    (->> (-> (h/select :id :first_name :last_name :email :password)
             (h/from :users)
             (h/where [:= :email email])
             (h/limit 1))
         (query db-spec)
         first))

  ; Entities
  (create-entity
    [_ entity]
    (insert db-spec :entities entity :name
                                     :user-id
                                     :monitored-account-ids))

  (select-entities
    [_ user-id]
    (query db-spec (-> (h/select :*)
                       (h/from :entities)
                       (h/where [:= :user_id user-id])
                       (h/order-by :name))))

  (find-entity-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :entities id)))

  (update-entity
    [_ entity]
    (let [sql (sql/format (-> (h/update :entities)
                              (h/sset (->update-set entity
                                                    :name
                                                    :monitored-account-ids))
                              (h/where [:= :id (:id entity)])))]
      (jdbc/execute! db-spec sql)))

  (delete-entity
    [_ id]
    (jdbc/delete! db-spec :entities ["id = ?" id]))

  ; Accounts
  (create-account
    [_ account]
    (insert db-spec :accounts account :name
                                      :type
                                      :entity-id
                                      :parent-id
                                      :balance))

  (find-account-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :accounts id)))

  (find-account-by-entity-id-and-name
    [_ entity-id account-name]
    (first (query db-spec (-> (h/select :*)
                       (h/from :accounts)
                       (h/where [:and
                                 [:= :entity_id entity-id]
                                 [:= :name account-name]])
                       (h/limit 1)))))

  (select-accounts-by-entity-id
    [_ entity-id]
    (query db-spec (-> (h/select :*)
                       (h/from :accounts)
                       (h/where [:= :entity_id entity-id]))))

  (update-account
    [_ account]
    (let [sql (sql/format (-> (h/update :accounts)
                              (h/sset (->update-set account
                                                    :name
                                                    :type
                                                    :parent-id
                                                    :balance))
                              (h/where [:= :id (:id account)])))]
      (jdbc/execute! db-spec sql)))

  (delete-account
    [_ id]
    (jdbc/delete! db-spec :accounts ["id = ?" id]))

  (select-accounts-by-name
    [_ entity-id name]
    (query db-spec (-> (h/select :*)
                       (h/from :accounts)
                       (h/where [:and
                                 [:= :entity_id entity-id]
                                 [:= :name name]]))))

  ; Transactions
  (select-transactions-by-entity-id
    [this entity-id]
    (.select-transactions-by-entity-id this entity-id {}))

  (select-transactions-by-entity-id
    [_ entity-id options]
    (let [sql (-> (h/select :*)
                  (h/from :transactions)
                  (h/where [:= :entity-id entity-id])
                  (h/order-by [:transaction-date :desc])
                  (append-paging options))]
      (query db-spec sql)))

  (count-transactions-by-entity-id
    [_ entity-id]
    (query-scalar db-spec
                  (-> (h/select :%count.*)
                      (h/from :transactions)
                      (h/where [:= :entity-id entity-id]))))

  (create-transaction
    [_ transaction]
    (insert db-spec :transactions transaction :entity-id
                                              :description
                                              :transaction-date))

  (find-transaction-by-id
    [_ id]
    (->> (-> (h/select :*)
            (h/from :transactions)
            (h/where [:= :id id])
            (h/limit 1))
        (query db-spec )
        first))

  (delete-transaction
    [_ id]
    (jdbc/delete! db-spec :transactions ["id = ?" id]))

  (update-transaction
    [_ transaction]
    (let [sql (sql/format (-> (h/update :transactions)
                              (h/sset (->update-set
                                        transaction
                                        :description
                                        :transaction-date))
                              (h/where [:= :id (:id transaction)])))]
      (jdbc/execute! db-spec sql)))

  ; Transaction Items
  (create-transaction-item
    [_ transaction-item]
    (insert db-spec :transaction_items transaction-item :transaction-id
                                                        :account-id
                                                        :action
                                                        :amount
                                                        :index
                                                        :balance))

  (select-transaction-items-by-transaction-id
    [_ transaction-id]
    (query db-spec (-> (transaction-item-base-query)
                       (h/where [:= :transaction_id transaction-id])
                       (h/order-by [:action :desc] [:amount :desc]))))

  (select-transaction-items-by-account-id
    [this account-id]
    (.select-transaction-items-by-account-id this account-id {}))

  (select-transaction-items-by-account-id
    [_ account-id options]
    (let [sql (-> (transaction-item-base-query)
                  (h/where [:= :i.account_id account-id])
                  (h/order-by [:i.index :desc])
                  (append-paging options))
          sql (cond-> sql
                (contains? options :reconciled?)
                (h/merge-where (if (:reconciled? options)
                                 [:= :r.status "completed"]
                                 [:or [:= :r.status nil]
                                      [:= :r.status "new"]])))]
      (query db-spec sql)))

  (count-transaction-items-by-account-id
    [this account-id]
    (query-scalar db-spec (-> (h/select :%count.*)
                              (h/from :transaction_items)
                              (h/where [:= :account_id account-id]))))

  (select-transaction-items-by-account-id-and-starting-index
    [_ account-id index]
    (query db-spec (-> (h/select :*)
                      (h/from :transaction_items)
                      (h/where [:and
                                [:= :account_id account-id]
                                [:>= :index index]])
                      (h/order-by [:index :desc]))))

  (select-transaction-items-by-account-id-on-or-after-date
    [_ account-id transaction-date]
    (query db-spec (-> (h/select :i.*)
                      (h/from [:transaction_items :i])
                      (h/join [:transactions :t] [:= :t.id :i.transaction-id])
                      (h/where [:and
                                [:= :i.account_id account-id]
                                [:>= :t.transaction_date transaction-date]])
                      (h/order-by :index))))

  (find-transaction-item-by-id
    [_ id]
    (->> (-> (h/select :*)
            (h/from :transaction_items)
            (h/where [:= :id id])
            (h/limit 1))
        (query db-spec)
        first))

  (find-transaction-items-by-ids
    [_ ids]
    (->> (-> (h/select :*)
             (h/from :transaction_items)
             (h/where [:in :id ids]))
        (query db-spec)))

  (select-transaction-items-preceding-date
    [_ account-id transaction-date]
    (query db-spec (-> (h/select :i.*)
                      (h/from [:transaction_items :i])
                      (h/join [:transactions :t] [:= :t.id :i.transaction-id])
                      (h/where [:and
                                [:= :i.account-id account-id]
                                [:< :t.transaction-date transaction-date]])
                      (h/order-by [:t.transaction-date :desc] [:i.index :desc])
                      (h/limit 2))))

  (find-last-transaction-item-on-or-before
    [_ account-id transaction-date]
    (first (query db-spec (-> (h/select :i.*)
                              (h/from [:transaction_items :i])
                              (h/join [:transactions :t] [:= :t.id :i.transaction_id])
                              (h/where [:and
                                        [:= :i.account_id account-id]
                                        [:<= :t.transaction_date transaction-date]])
                              (h/order-by [:t.transaction_date :desc] [:i.index :desc])
                              (h/limit 1)))))

  (update-transaction-item
    [_ transaction-item]
    (let [sql (sql/format (-> (h/update :transaction_items)
                              (h/sset (->update-set transaction-item
                                                    :amount
                                                    :action
                                                    :index
                                                    :balance
                                                    :account-id))
                              (h/where [:= :id (:id transaction-item)])))]
      (try
        (jdbc/execute! db-spec sql)
        (catch BatchUpdateException e
          (pprint {:sql sql
                   :batch-update-exception (.getNextException e)})))))

  (update-transaction-item-index-and-balance
    [_ transaction-item]
    (let [sql (sql/format (-> (h/update :transaction_items)
                                (h/sset (->update-set transaction-item
                                                      :index
                                                      :balance))
                                (h/where [:and
                                          [:= :id (:id transaction-item)]
                                          [:or
                                           [:!= :balance (:balance transaction-item)]
                                           [:!= :index (:index transaction-item)]]])))]
        (try
          (jdbc/execute! db-spec sql)
          (catch BatchUpdateException e
            (pprint {:sql sql
                    :batch-update-exception (.getNextException e)})))))

  (delete-transaction-item
    [_ id]
    (jdbc/delete! db-spec :transaction_items ["id = ?" id]))

  (delete-transaction-items-by-transaction-id
    [_ transaction-id]
    (jdbc/delete! db-spec
                  :transaction_items
                  ["transaction_id = ?" transaction-id]))

  (set-transaction-items-reconciled
    [_ reconciliation-id transaction-item-ids]
    (jdbc/execute! db-spec (-> (h/update :transaction_items)
                               (h/sset {:reconciliation_id reconciliation-id})
                               (h/where [:in :id transaction-item-ids])
                               sql/format)))

  ; Reconciliations
  (create-reconciliation
    [_ reconciliation]
    (insert db-spec :reconciliations reconciliation :account-id
                                                    :balance
                                                    :end-of-period
                                                    :status))

  (find-reconciliation-by-id
    [_ id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :reconciliations)
                              (h/where [:= :id id])
                              (h/limit 1)))))

  (select-reconciliations-by-account-id
    [_ account-id]
    (query db-spec (-> (h/select :*)
                       (h/from :reconciliations)
                       (h/where [:= :account-id account-id]))))

  (find-last-complete-reconciliation-by-account-id
    [_ account-id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :reconciliations)
                              (h/where [:and
                                        [:= :account-id account-id]
                                        [:= :status "completed"]])
                              (h/order-by [:end_of_period :desc])
                              (h/limit 1)))))

  (find-new-reconciliation-by-account-id
    [_ account-id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :reconciliations)
                              (h/where [:and [:= :status "new"]
                                        [:= :account-id account-id]])
                              (h/limit 1)))))

  (update-reconciliation
    [_ reconciliation]
    (let [sql (sql/format (-> (h/update :reconciliations)
                              (h/sset (->update-set reconciliation
                                                    :account-id
                                                    :balance
                                                    :status
                                                    :end-of-period))
                              (h/where [:= :id (:id reconciliation)])))]
      (jdbc/execute! db-spec sql)))

  ; Budgets
  (create-budget
    [_ budget]
    (insert db-spec :budgets budget :entity-id
                                    :name
                                    :period
                                    :period-count
                                    :start-date
                                    :end-date))

  (find-budget-by-id
    [_ id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :budgets)
                              (h/where [:= :id id])
                              (h/limit 1)))))

  (find-budget-by-date
    [_ date]
    (first (query db-spec (-> (h/select :*)
                              (h/from :budgets)
                              (h/where [:and
                                        [:<= :start-date date]
                                        [:>= :end-date date]])
                              (h/limit 1)))))

  (select-budgets-by-entity-id
    [_ entity-id]
    (query db-spec (-> (h/select :*)
                       (h/from :budgets)
                       (h/where [:= :entity_id entity-id]))))

  (update-budget
    [_ budget]
    (let [sql (sql/format (-> (h/update :budgets)
                              (h/sset (->update-set budget
                                                    :name
                                                    :period
                                                    :period-count
                                                    :start-date
                                                    :end-date))
                              (h/where [:= :id (:id budget)])))]
      (jdbc/execute! db-spec sql)))

  (delete-budget
    [_ id]
    (jdbc/delete! db-spec :budgets ["id = ?" id]))

  ; Budget items
  (create-budget-item
    [_ budget-item]
    (insert db-spec :budget_items budget-item :budget-id
                                              :account-id
                                              :periods))

  (update-budget-item
    [_ budget-item]
    (let [sql (sql/format (-> (h/update :budget_items)
                              (h/sset (->update-set budget-item
                                                    :account-id
                                                    :periods))
                              (h/where [:= :id (:id budget-item)])))]
      (jdbc/execute! db-spec sql)))

  (find-budget-item-by-id
    [_ id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :budget_items)
                              (h/where [:= :id id])
                              (h/limit 1)))))

  (select-budget-items-by-budget-id
    [_ budget-id]
    (query db-spec (-> (h/select :*)
                       (h/from :budget_items)
                       (h/where [:= :budget_id budget-id]))))

  ; Database Transaction
  (with-transaction
    [_ func]
    (jdbc/with-db-transaction [trans db-spec {:isolation :serializable :read-only false}]
      (func (SqlStorage. trans)))))
