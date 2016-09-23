(ns clj-money.models.storage.sql-storage
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
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

(deftype SqlStorage [db-spec]
  Storage

  ; Users
  (create-user
    [_ user]
    (try
      (->> user
           (jdbc/insert! db-spec :users)
           first)
      (catch java.sql.BatchUpdateException e
        (log/error (format "Unable to insert user %s: %s"
                        user
                        (.getMessage (.getNextException e))))
        (throw e))))

  (select-users
    [_]
    (let [sql (sql/format (-> (h/select :first_name :last_name :email)
                              (h/from :users)))]
      (jdbc/query db-spec sql)))

  (find-user-by-email
    [this email]
    (let [sql (sql/format (-> (h/select :id :first_name :last_name :email :password)
                              (h/from :users)
                              (h/where [:= :email email])))]
      (->> sql
           (jdbc/query db-spec)
           first)))

  (user-exists-with-email?
    [this email]
    (exists? db-spec :users [:= :email email]))

  ; Entities
  (create-entity
    [_ entity]
    (->> entity
         (jdbc/insert! db-spec :entities)
         first))
  (select-entities
    [_ user-id]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :entities)
                              (h/where [:= :user_id user-id])
                              (h/order-by :name)))]
      (jdbc/query db-spec sql)))

  (entity-exists-with-name?
    [_ user-id name]
    (exists? db-spec :entities [:and
                                [:= :user_id user-id]
                                [:= :name name]]))

  (find-entity-by-id
    [_ id]
    (jdbc/get-by-id db-spec :entities id))

  (update-entity
    [_ entity]
    (let [sql (sql/format (-> (h/update :entities)
                              (h/sset (select-keys entity [:name]))
                              (h/where [:= :id (:id entity)])))]
      (jdbc/execute! db-spec sql)))

  ; Accounts
  (create-account
    [_ account]
    (->> account
         (jdbc/insert! db-spec :accounts)
         first))

  (select-accounts
    [_]
    (let [sql (sql/format (-> (h/select :*)
                              (h/from :accounts)))]
      (jdbc/query db-spec sql))))
