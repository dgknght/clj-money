(ns clj-money.models.users
  (:require [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [cemerick.friend.credentials :refer [hash-bcrypt
                                                 bcrypt-verify]]
            [schema.core :as s]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.validation :refer :all]
            [clj-money.models.storage :refer [create-user
                                              select-users
                                              find-user-by-email
                                              user-exists-with-email?]]))

(defn prepare-user-for-insertion
  "Prepares a user record to be saved in the database"
  [user]
  (update-in user [:password] hash-bcrypt))

(defn prepare-user-for-return
  "Prepares a user record for return to the caller"
  [user]
  (dissoc user :password))

(def EmailPattern #"\A[\w\.-_]+@[\w\.-_]+\.\w{2,4}\z")

(def NewUser
  "Schema for a new user"
  {:first-name s/Str
   :last-name s/Str
   :email (s/pred (partial re-matches EmailPattern) "invalid format")
   :password s/Str})

(defn- validate-new-user
  [storage user]
  (validate-model user [(partial apply-schema NewUser)
                        (fn [model]
                          (if (user-exists-with-email? storage (:email model))
                            [[:email "Email is already taken"]]))]))

(defn create
  "Creates a new user record"
  [storage-spec user]
  (let [s (storage storage-spec)
        validated (validate-new-user s user)]
    (if (has-error? validated)
      validated
      (->> user
           prepare-user-for-insertion
           (create-user s)
           prepare-user-for-return))))

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

(defn full-name
  "Returns the user's full name"
  [user]
  (format "%s %s" (:first-name user) (:last-name user)))
