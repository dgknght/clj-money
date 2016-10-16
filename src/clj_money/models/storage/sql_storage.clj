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
  [db-spec table model]
  (->> model
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

(deftype SqlStorage [db-spec]
  Storage

  ; Users
  (create-user
    [_ user]
    (insert db-spec :users user))

  (select-users
    [_]
    (let [sql (sql/format (-> (h/select :first_name :last_name :email)
                              (h/from :users)))]
      (->> (jdbc/query db-spec sql)
           (map ->clojure-keys))))

  (find-user-by-email
    [this email]
    (let [sql (sql/format (-> (h/select :id :first_name :last_name :email :password)
                              (h/from :users)
                              (h/where [:= :email email])))]
      (->> sql
           (jdbc/query db-spec)
           first
           ->clojure-keys)))

  (user-exists-with-email?
    [this email]
    (exists? db-spec :users [:= :email email]))

  ; Entities
  (create-entity
    [_ entity]
    (->> entity
         ->sql-keys
         (jdbc/insert! db-spec :entities)
         first
         ->clojure-keys))
  (select-entities
    [_ user-id]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :entities)
                              (h/where [:= :user_id user-id])
                              (h/order-by :name)))]
      (map ->clojure-keys (jdbc/query db-spec sql))))

  (entity-exists-with-name?
    [_ user-id name]
    (exists? db-spec :entities [:and
                                [:= :user_id user-id]
                                [:= :name name]]))

  (find-entity-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :entities id)))

  (update-entity
    [_ entity]
    (let [sql (sql/format (-> (h/update :entities)
                              (h/sset (->update-set entity :name))
                              (h/where [:= :id (:id entity)])))]
      (jdbc/execute! db-spec sql)))

  (delete-entity
    [_ id]
    (jdbc/delete! db-spec :entities ["id = ?" id]))

  ; Accounts
  (create-account
    [_ account]
    (insert db-spec :accounts account))

  (find-account-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :accounts id)))

  (select-accounts-by-entity-id
    [_ entity-id]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :accounts)
                              (h/where [:= :entity_id entity-id])))]
      (map ->clojure-keys (jdbc/query db-spec sql))))

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

  (find-accounts-by-name
    [_ entity-id name]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :accounts)
                              (h/where [:and
                                        [:= :entity_id entity-id]
                                        [:= :name name]])))]
      (->> (jdbc/query db-spec sql)
          (map ->clojure-keys))))

  ; Transactions
  (create-transaction
    [_ transaction]
    (insert db-spec :transactions transaction))

  (find-transaction-by-id
      [_ id]
      (let [sql (sql/format (-> (h/select :*)
                                (h/from :transactions)
                                (h/where [:= :id id])
                                (h/limit 1)))]
        (->> (jdbc/query db-spec sql)
            (map ->clojure-keys)
            first)))

  (delete-transaction
    [_ id]
    (jdbc/delete! db-spec :transactions ["id = ?" id]))

  ; Transaction Items
  (create-transaction-item
    [_ transaction-item]
    (insert db-spec :transaction_items transaction-item))

  (select-transaction-items-by-transaction-id
    [_ transaction-id]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :transaction_items)
                              (h/where [:= :transaction_id transaction-id])))]
            (->> (jdbc/query db-spec sql)
                (map ->clojure-keys))))

  (select-transaction-items-by-account-id
    [_ account-id]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :transaction_items)
                              (h/where [:= :account_id account-id])
                              (h/order-by [:index :desc])))]
            (->> (jdbc/query db-spec sql)
                (map ->clojure-keys))))

  (select-transaction-items-by-account-id-and-starting-index
    [_ account-id index]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :transaction_items)
                              (h/where [:and
                                        [:= :account_id account-id]
                                        [:>= :index index]])
                              (h/order-by [:index :desc])))]
      (->> (jdbc/query db-spec sql)
          (map ->clojure-keys))))

  (find-transaction-item-by-index
    [_ account-id index]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :transaction_items)
                              (h/where [:= :index index])
                              (h/limit 1)))]
      (->> (jdbc/query db-spec sql)
          first
          ->clojure-keys)))

  (find-transaction-item-preceding-date
    [_ account-id transaction-date]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from [:transaction_items :i])
                              (h/join [:transactions :t] [:= :t.id :i.transaction-id])
                              (h/where [:and
                                        [:= :i.account-id account-id]
                                        [:< :t.transaction-date (tc/to-sql-date transaction-date)]])
                              (h/order-by [:t.transaction-date :desc] [:i.index :desc])
                              (h/limit 1)))]
      (->> (jdbc/query db-spec sql)
          first
          ->clojure-keys)))

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

  (delete-transaction-items-by-transaction-id
    [_ transaction-id]
    (jdbc/delete! db-spec
                  :transaction_items
                  ["transaction_id = ?" transaction-id])))
