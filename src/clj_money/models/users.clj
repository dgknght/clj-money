(ns clj-money.models.users
  (:require [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [cemerick.friend.credentials :refer [hash-bcrypt
                                                 bcrypt-verify]]
            [schema.core :as s]
            [clj-money.models.helpers :refer [storage validate-model]]
            [clj-money.models.storage :refer [create-user
                                              select-users
                                              find-user-by-email]]))

(defn prepare-user-for-insertion
  "Prepares a user record to be saved in the database"
  [user]
  (-> user
      (update-in [:password] hash-bcrypt)
      (rename-keys {:first-name :first_name
                    :last-name :last_name})))

(defn prepare-user-for-return
  "Prepares a user record for return to the caller"
  [user]
  (-> user
      (dissoc :password)
      (rename-keys {:first_name :first-name
                    :last_name :last-name})))

(def EmailPattern #"\A[\w\.-_]+@[\w\.-_]+\.\w{2,4}\z")

(def NewUser
  "Schema for a new user"
  {:first-name s/Str
   :last-name s/Str
   :email (s/pred (partial re-matches EmailPattern) "invalid format")
   :password s/Str})

(defn- validate-new-user
  [user]
  (validate-model user NewUser "user"))

(defn create
  "Creates a new user record"
  [storage-spec user]
  (->> user
       validate-new-user
       prepare-user-for-insertion
       (create-user (storage storage-spec))
       prepare-user-for-return))

(defn select
  "Lists the users in the database"
  [storage-spec]
  (->> storage-spec
       storage
       select-users
       (map prepare-user-for-return)))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [storage-spec {:keys [username password]}]
  (let [user (find-user-by-email (storage storage-spec) username)]
    (when (and user (bcrypt-verify password (:password user)))
      (-> user
          prepare-user-for-return
          (assoc :type :cemerick.friend/auth
                 :identity (:id user)
                 :roles #{:user})))))
