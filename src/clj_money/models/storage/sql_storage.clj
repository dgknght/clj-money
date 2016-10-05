(ns clj-money.models.storage.sql-storage
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-time.jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-money.models.storage :refer [Storage]]))

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
  (update-keys model ->clojure-key))

(defn- insert
  "Inserts a record into the specified table"
  [db-spec table model]
  (->> model
       ->sql-keys
       (jdbc/insert! db-spec table)
       first
       ->clojure-keys))

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
                              (h/sset (->sql-keys (select-keys entity [:name])))
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
                              (h/sset (->sql-keys (select-keys account [:name
                                                                        :type
                                                                        :parent-id])))
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
                (map ->clojure-keys)))))
