(ns clj-money.models.users
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [cemerick.friend.credentials :refer [hash-bcrypt]]))

(defn prepare-users-for-insertion
  "Prepares user data to be saved in the database"
  [users]
  (map #(update-in % [:password] hash-bcrypt) users))

(defn create
  "Creates a new user record"
  [data-store user]
  (let [values (prepare-users-for-insertion [user])
        sql (sql/format (-> (h/insert-into :users)
                            (h/values values)))]
    (try
      (jdbc/with-db-connection [db data-store]
        (let [id (jdbc/execute! db sql)]
          (find db id)))
      (catch java.sql.BatchUpdateException e
        (log/error (str "Unable to insert user "
                        user
                        ": "
                        (.getMessage (.getNextException e))))))))

(defn find
  "Returns a user by ID"
  [data-store id]
  (let [sql (sql/format (-> (h/select :first_name :last_name :email)
                            (h/from :users)
                            (h/where {:id id})))]

    (pprint sql)

    (first(jdbc/query data-store sql))))

(defn select
  "Lists the users in the database"
  [data-store]
  (let [sql (sql/format (-> (h/select :first_name :last_name :email)
                            (h/from :users)))]
    (jdbc/query data-store sql)))
