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
  (let [user (validate-new-user user)]
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
                        (.getMessage (.getNextException e))))))))

(defn select
  "Lists the users in the database"
  [data-store]
  (let [sql (sql/format (-> (h/select :first_name :last_name :email)
                            (h/from :users)))]
    (jdbc/query data-store sql)))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [data-store {:keys [username password]}]
  (let [sql (sql/format (-> (h/select :id :first_name :last_name :email :password)
                            (h/from :users)
                            (h/where [:= :email username])))
        user (first (jdbc/query data-store sql))]
    (when (and user (bcrypt-verify password (:password user)))
      (-> user
          (dissoc :password)
          (assoc :type :cemerick.friend/auth
                 :identity (:id user)
                 :roles #{:user})))))
