(ns clj-money.models.users
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clj-time.core :as t]
            [clj-money.util :refer [to-sql-date]]
            [buddy.hashers :as hashers]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]
            [clj-money.models.helpers :refer [create-fn
                                              update-fn
                                              throw-if-nil]]
            [clj-money.validation :as validation])
  (:import java.util.UUID))

(defn- before-save
  "Prepares a user record to be saved in the database"
  [user & _]
  (-> user
    (models/tag ::models/user)
    (update-in [:password] hashers/derive)))

(defn- after-read
  [user & _]
  (models/tag user ::models/user))

(s/def ::first-name validation/non-empty-string?)
(s/def ::last-name validation/non-empty-string?)
(s/def ::password validation/non-empty-string?)
(s/def ::email validation/email?)
(s/def ::new-user (s/keys :req-un [::first-name ::last-name ::password ::email]))
(s/def ::existing-user (s/keys :req-un [::id ::first-name ::last-name ::email] :opt-un [::password]))

(defn find
  ([storage-spec criteria]
   (find storage-spec criteria {}))
  ([storage-spec criteria options]
   {:pre [(map? criteria) (map? options)]}
   (with-storage [s storage-spec]
     (->> (storage/select s
                          (models/tag criteria :user)
                          (merge options {:limit 1}))
          first))))

(defn find-by-email
  "Returns the user having the specified email"
  [storage-spec email]
  (find storage-spec {:email email}))

(defn- email-is-unique?
  [storage user]
  (nil? (find-by-email storage (:email user))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial email-is-unique? storage)
                           [:email]
                           "Email is already taken")])

(def create
  (create-fn {:spec ::new-user
              :rules-fn validation-rules
              :create (fn [user s] (storage/create s user))
              :after-read after-read
              :before-save before-save}))

(defn select
  "Lists the users in the database"
  ([storage-spec]
   (select storage-spec {}))
  ([storage-spec criteria]
   (select storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (storage/select s (models/tag criteria ::models/user) options))))

(defn find-by-id
  "Returns the user having the specified id"
  [storage-spec id]
  (find storage-spec {:id id}))

(defn find-by-token
  "Returns the user having the specified, unexpired password reset token"
  [storage-spec token]
  (find storage-spec {:password-reset-token token
                      :token-expires-at [:> (to-sql-date (t/now))]}))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [storage-spec {:keys [username password]}]
  (with-storage [s storage-spec]
    (let [user (find s {:email username} {:include-password? true})]
      (when (and user (hashers/check password (:password user)))
        (-> user
            (dissoc :password)
            (assoc :type :cemerick.friend/auth
                   :identity (:id user)
                   :roles #{:user}))))))

(defn full-name
  "Returns the user's full name"
  [user]
  (format "%s %s" (:first-name user) (:last-name user)))

(def update
  (update-fn {:update (fn [user s] (storage/update s user))
              :find find-by-id
              :spec ::existing-user}))

(defn create-password-reset-token
  "Creates and sets the user's password reset token and expiration,
  and returns the token"
  [storage-spec user]
  (let [token (string/replace (.toString (UUID/randomUUID)) "-" "")]
    (update storage-spec (assoc user :password-reset-token token
                                     :token-expires-at (-> 24 t/hours t/from-now to-sql-date)))
    token))

(defn reset-password
  "Changes the user's password to the specified value
  and invalidates the token"
  [storage-spec token password]
  (with-storage [s storage-spec]
    (let [user (-> (find-by-token s token)
                   throw-if-nil
                   (assoc :password password
                          :password-reset-token nil
                          :token-expires-at nil)
                   before-save)]
      (update s user))))

(defn find-or-create-from-profile
  [storage-spec profile]
  (let [user (find storage-spec (select-keys profile [:email]))]
    user))
