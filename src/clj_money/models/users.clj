(ns clj-money.models.users
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [cemerick.friend.credentials :refer [hash-bcrypt
                                                 bcrypt-verify]]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [schema.utils :as sutils]))

(defprotocol Storage
  "Provides data storage services for the application"
  (create-user
    [this user]
    "Creates a new user record")
  (select-users
    [this]
    "Returns all of the users in the system matching the specified criteria")
  (find-user-by-email
    [this email]
    "Returns the user having the specified email"))

(deftype SqlStorage [db-spec]
  Storage

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
           first))))

(defn prepare-user-for-insertion
  "Prepares a user record to be saved in the database"
  [user]
  (-> user
      (update-in [:password] hash-bcrypt)))

(defn prepare-user-for-return
  "Prepares a user record for return to the caller"
  [user]
  (dissoc user :password))

(def EmailPattern #"\A[\w\.-_]+@[\w\.-_]+\.\w{2,4}\z")

(def NewUser
  "Schema for a new user"
  {:first_name s/Str
   :last_name s/Str
   :email (s/pred (partial re-matches EmailPattern) "invalid format")
   :password s/Str})

(defn nil-matcher
  [schema]
  (when (= s/Str schema)
    (coerce/safe
      (fn [value]
        (if (and (string? value) (= 0 (count value)))
          nil
          value)))))

(defn- validate-new-user
  [user]
  (let [coercer (coerce/coercer NewUser
                                nil-matcher)
        result (coercer user)]
    (if (sutils/error? result)
      (throw (ex-info "The user is not valid."
                      (merge result
                             {:schema NewUser
                              :value user
                              :type :schema.core/error})))
      result)))

(defn create
  "Creates a new user record"
  [data-store user]
  (->> user
       validate-new-user
       prepare-user-for-insertion
       (create-user (SqlStorage. data-store))
       prepare-user-for-return))

(defn select
  "Lists the users in the database"
  [data-store]
  (select-users (SqlStorage. data-store)))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [data-store {:keys [username password]}]
  (let [user (find-user-by-email (SqlStorage. data-store) username)]
    (when (and user (bcrypt-verify password (:password user)))
      (-> user
          (dissoc :password)
          (assoc :type :cemerick.friend/auth
                 :identity (:id user)
                 :roles #{:user})))))
