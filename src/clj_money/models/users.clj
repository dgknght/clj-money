(ns clj-money.models.users
  (:require [clojure.tools.logging :as log]
            [clojure.spec :as s]
            [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [cemerick.friend.credentials :refer [hash-bcrypt
                                                 bcrypt-verify]]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.validation :as validation]
            [clj-money.models.storage :refer [create-user
                                              select-users
                                              find-user-by-email]]))

(defn prepare-user-for-insertion
  "Prepares a user record to be saved in the database"
  [user]
  (update-in user [:password] hash-bcrypt))

(defn prepare-user-for-return
  "Prepares a user record for return to the caller"
  [user]
  (dissoc user :password))

(def EmailPattern #"\A[\w\.-_]+@[\w\.-_]+\.\w{2,4}\z")

(s/def ::first-name validation/non-empty-string?)
(s/def ::last-name validation/non-empty-string?)
(s/def ::password validation/non-empty-string?)
(s/def ::email (s/and string? (partial re-matches EmailPattern)))
(s/def ::new-user (s/keys :req-un [::first-name ::last-name ::password ::email]))

(defn- email-is-unique?
  [storage user]
  (nil? (find-user-by-email storage (:email user))))

(defn create
  "Creates a new user record"
  [storage-spec user]
  (with-storage [s storage-spec]
    (let [unique-email-rule (validation/create-rule (partial email-is-unique? s)
                                                    [:email]
                                                    "Email is already taken")
          validated (validation/validate ::new-user
                                         user
                                         unique-email-rule)]
      (if (validation/valid? validated)
        (->> user
             prepare-user-for-insertion
             (create-user s)
             prepare-user-for-return)
        validated))))

(defn select
  "Lists the users in the database"
  [storage-spec]
  (with-storage [s storage-spec]
    (->> s
         select-users
         (map prepare-user-for-return))))

(defn find-by-email
  "Returns the user having the specified email"
  [storage-spec email]
  (with-storage [s storage-spec]
    (find-user-by-email s email)))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [storage-spec {:keys [username password]}]
  (with-storage [s storage-spec]
    (let [user (find-user-by-email s username)]
      (when (and user (bcrypt-verify password (:password user)))
        (-> user
            prepare-user-for-return
            (assoc :type :cemerick.friend/auth
                   :identity (:id user)
                   :roles #{:user}))))))

(defn full-name
  "Returns the user's full name"
  [user]
  (format "%s %s" (:first-name user) (:last-name user)))
