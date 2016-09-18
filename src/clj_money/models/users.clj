(ns clj-money.models.users
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [cemerick.friend.credentials :refer [hash-bcrypt]]
            [schema.core :as s]))

(defn prepare-user-for-insertion
  "Prepares a user record to be saved in the database"
  [user]
  (-> user
      (update-in [:password] hash-bcrypt)))

(def EmailPattern #"\A[\w\.-_]+@[\w\.-_]+\.\w{2,4}\z")

(def NewUser
  "Schema for a new user"
  {:first_name s/Str
   :last_name s/Str
   :email (s/pred (partial re-matches EmailPattern) "Email must be a valid email address")
   :password s/Str})

(defn create
  "Creates a new user record"
  [data-store user]
  (s/validate NewUser user)
  (try
    (dissoc (->> user
                 prepare-user-for-insertion
                 (jdbc/insert! data-store :users)
                 first)
            :password)
    (catch java.sql.BatchUpdateException e
      (log/error (str "Unable to insert user "
                      user
                      ": "
                      (.getMessage (.getNextException e)))))))

(defn select
  "Lists the users in the database"
  [data-store]
  (let [sql (sql/format (-> (h/select :first_name :last_name :email)
                            (h/from :users)))]
    (jdbc/query data-store sql)))
