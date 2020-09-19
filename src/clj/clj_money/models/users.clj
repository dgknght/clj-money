(ns clj-money.models.users
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [slingshot.slingshot :refer [throw+]]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-sql-date]]
            [environ.core :refer [env]]
            [buddy.hashers :as hashers]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.util :refer [assoc-if
                                    ->id]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]
            [clj-money.validation :as validation :refer [with-validation]])
  (:import java.util.UUID))

(defn- before-save
  [user]
  (-> user
      (tag ::models/user)
      (update-in [:password] hashers/derive)))

(defn- after-read
  [user]
  (tag user ::models/user))

(s/def ::first-name validation/non-empty-string?)
(s/def ::last-name validation/non-empty-string?)
(s/def ::password validation/non-empty-string?)
(s/def ::email validation/email?)
(s/def ::new-user (s/keys :req-un [::first-name ::last-name ::password ::email]))
(s/def ::existing-user (s/keys :req-un [::id ::first-name ::last-name ::email] :opt-un [::password]))

(defn search
  [criteria options]
  (with-storage (env :db)
    (map after-read
         (storage/select (tag criteria ::models/user)
                         options))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   {:pre [(map? criteria) (map? options)]}
   (first (search criteria
                  (merge options {:limit 1})))))

(defn find-by-email
  "Returns the user having the specified email"
  [email]
  (find-by {:email email}))

(defn- email-is-unique?
  [{:keys [email id]}]
  (-> {:email email}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))

(def validation-rules
  [(validation/create-rule email-is-unique?
                           [:email]
                           "Email is already taken")])

(defn create
  [user]
  (with-storage (env :db)
    (with-validation user ::new-user validation-rules
      (-> user
          before-save
          storage/create
          after-read))))

(defn select
  "Lists the users in the database"
  ([]
   (select {}))
  ([criteria]
   (select criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (storage/select (tag criteria ::models/user) options))))

(defn find
  "Returns the user having the specified id"
  [id-or-entity]
  (find-by {:id (->id id-or-entity)}))

(defn find-by-token
  "Returns the user having the specified, unexpired password reset token"
  [token]
  (find-by {:password-reset-token token
            :token-expires-at [:> (to-sql-date (t/now))]}))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [{:keys [username password]}]
  (with-storage (env :db)
    (when-let [user (find-by {:email username} {:include-password? true})]
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
  [user]
  (with-storage (env :db)
    (with-validation user ::existing-user validation-rules
      (-> user
          before-save
          storage/update)
      (find user))))

(defn create-password-reset-token
  "Creates and sets the user's password reset token and expiration,
  and returns the token"
  [user]
  (let [token (string/replace (.toString (UUID/randomUUID)) "-" "")]
    (-> user
        (tag ::models/user)
        (assoc :password-reset-token token
               :token-expires-at (-> 24 t/hours t/from-now to-sql-date))
        update) 
    token))

(defn reset-password
  "Changes the user's password to the specified value
  and invalidates the token"
  [token password]
  (if-let [user (find-by-token token)]
    (-> user
        (assoc :password password
               :password-reset-token nil
               :token-expires-at nil)
        update)
    (throw+ {:type ::models/not-found})))

(defn find-or-create-from-profile
  [profile]
  (find-by (select-keys profile [:email])))
