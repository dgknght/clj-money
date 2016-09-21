(ns clj-money.models.storage.sql-storage
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-money.models.storage :refer [Storage]]))

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
