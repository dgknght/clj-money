(ns clj-money.models.users
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [slingshot.slingshot :refer [throw+]]
            [java-time.api :as t]
            [buddy.hashers :as hashers]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?
                                          update-in-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.db :as db]
            [clj-money.models :as models])
  (:import java.util.UUID))

(declare find-by)

(defn- email-is-unique?
  [{:keys [id] :as user}]
  (-> (select-keys user [:user/email])
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))
(v/reg-spec email-is-unique? {:message "%s is already in use"
                              :path [:user/email]})

(s/def :user/first-name (s/and string?
                           present?))
(s/def :user/last-name (s/and string?
                          present?))
(s/def :user/password (s/and string?
                         present?))
(s/def :user/email (s/and string?
                      present?
                      v/email?))
(s/def ::user (s/and (s/keys :req [:user/first-name :user/last-name :user/email]
                             :opt [:user/password])
                     email-is-unique?))


(defn- before-save
  [user]
  (update-in-if user [:user/password] hashers/derive))

(defn select
  ([] (select {}))
  ([criteria] (select criteria {}))
  ([criteria options]
   (db/select (db/storage)
              (db/model-type criteria :user)
              options)))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   {:pre [(map? criteria) (map? options)]}
   (first (select criteria
                  (merge options {:limit 1})))))

(defn find
  "Returns the user having the specified id"
  [id-or-entity]
  (find-by {:id (->id id-or-entity)}))

(defn find-by-email
  "Returns the user having the specified email"
  [email]
  (find-by {:email email}))

(defn find-by-token
  "Returns the user having the specified, unexpired password reset token"
  [token]
  (find-by {:password-reset-token token
            :token-expires-at [:> (t/instant)]}))

(defn put
  [user]
  {:pre [(or (:id user) (:user/password user))]}
  (with-ex-validation user ::user
    (db/put (db/storage)
            [(before-save user)])))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [{:keys [username password]}]
  (when-let [user (find-by {:email username} {:include-password? true})]
    (when (hashers/check password (:password user))
      (-> user
          (dissoc :password)
          (assoc :type :cemerick.friend/auth
                 :identity (:id user)
                 :roles #{:user})))))

(defn full-name
  "Returns the user's full name"
  [user]
  (format "%s %s" (:first-name user) (:last-name user)))

(defn create-password-reset-token
  "Creates and sets the user's password reset token and expiration,
  and returns the token"
  [user]
  (let [token (string/replace (.toString (UUID/randomUUID)) "-" "")]
    (-> user
        (db/model-type :user)
        (assoc :password-reset-token token
               :token-expires-at (t/plus (t/instant) (t/hours 24)))
        put)
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
        put)
    (throw+ {:type ::models/not-found})))

(defn find-or-create-from-profile
  [profile]
  (find-by (select-keys profile [:email])))
