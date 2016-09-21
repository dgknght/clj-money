(ns clj-money.models.users
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [cemerick.friend.credentials :refer [hash-bcrypt
                                                 bcrypt-verify]]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [schema.utils :as sutils]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-user
                                              select-users
                                              find-user-by-email]]))

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
  [storage-spec user]
  (->> user
       validate-new-user
       prepare-user-for-insertion
       (create-user (storage storage-spec))
       prepare-user-for-return))

(defn select
  "Lists the users in the database"
  [storage-spec]
  (select-users (storage storage-spec)))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [storage-spec {:keys [username password]}]
  (let [user (find-user-by-email (storage storage-spec) username)]
    (when (and user (bcrypt-verify password (:password user)))
      (-> user
          (dissoc :password)
          (assoc :type :cemerick.friend/auth
                 :identity (:id user)
                 :roles #{:user})))))
