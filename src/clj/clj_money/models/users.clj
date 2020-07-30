(ns clj-money.models.users
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [slingshot.slingshot :refer [throw+]]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-sql-date]]
            [buddy.hashers :as hashers]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]
            [clj-money.validation :as validation :refer [with-validation]])
  (:import java.util.UUID))

(defn- before-save
  [user]
  (-> user
      (storage/tag ::models/user)
      (update-in [:password] hashers/derive)))

(defn- after-read
  [user]
  (storage/tag user ::models/user))

(s/def ::first-name validation/non-empty-string?)
(s/def ::last-name validation/non-empty-string?)
(s/def ::password validation/non-empty-string?)
(s/def ::email validation/email?)
(s/def ::new-user (s/keys :req-un [::first-name ::last-name ::password ::email]))
(s/def ::existing-user (s/keys :req-un [::id ::first-name ::last-name ::email] :opt-un [::password]))

(defn search
  [storage criteria options]
  (with-storage [s storage]
    (map after-read
         (storage/select s
                         (storage/tag criteria ::models/user)
                         options))))

(defn find
  ([storage-spec criteria]
   (find storage-spec criteria {}))
  ([storage-spec criteria options]
   {:pre [(map? criteria) (map? options)]}
   (first (search storage-spec
                  criteria
                  (merge options {:limit 1})))))

(defn find-by-email
  "Returns the user having the specified email"
  [storage-spec email]
  (find storage-spec {:email email}))

(defn- email-is-unique?
  [storage user]
  (let [query (select-keys user [:email])
        query (if-let [id (:id user)]
                (assoc query :id [:!= id])
                query)]
    (nil? (find storage query))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial email-is-unique? storage)
                           [:email]
                           "Email is already taken")])

(defn create
  [storage user]
  (with-storage [s storage]
    (with-validation user ::new-user (validation-rules s)
      (as-> user u
        (before-save u)
        (storage/create s u)
        (after-read u)))))

(defn select
  "Lists the users in the database"
  ([storage-spec]
   (select storage-spec {}))
  ([storage-spec criteria]
   (select storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (storage/select s (storage/tag criteria ::models/user) options))))

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
    (when-let [user (find s {:email username} {:include-password? true})]
      (when (hashers/check password (:password user))
        (-> user
            (dissoc :password)
            (assoc :type :cemerick.friend/auth
                   :identity (:id user)
                   :roles #{:user}))))))

(defn full-name
  "Returns the user's full name"
  [user]
  (format "%s %s" (:first-name user) (:last-name user)))

(defn update
  [storage user]
  (with-storage [s storage]
    (with-validation user ::existing-user (validation-rules s)
      (as-> user u
        (before-save u)
        (storage/update s u))
      (find-by-id s (:id user)))))

(defn create-password-reset-token
  "Creates and sets the user's password reset token and expiration,
  and returns the token"
  [storage-spec user]
  (let [token (string/replace (.toString (UUID/randomUUID)) "-" "")]
    (update storage-spec
            (assoc user :password-reset-token token
                   :token-expires-at (-> 24 t/hours t/from-now to-sql-date)))
    token))

(defn reset-password
  "Changes the user's password to the specified value
  and invalidates the token"
  [storage-spec token password]
  (with-storage [s storage-spec]
    (if-let [user (find-by-token s token)]
      (update s (assoc user  :password password
                       :password-reset-token nil
                       :token-expires-at nil))
      (throw+ {:type ::models/not-found}))))

(defn find-or-create-from-profile
  [storage-spec profile]
  (let [user (find storage-spec (select-keys profile [:email]))]
    user))
