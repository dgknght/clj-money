(ns clj-money.models.users
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [slingshot.slingshot :refer [throw+]]
            [java-time.api :as t]
            [buddy.hashers :as hashers]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?
                                          update-in-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models])
  (:import java.util.UUID))

(defn- email-is-unique?
  [{:keys [id] :as user}]
  (-> (select-keys user [:user/email])
      (assoc-if :id (when id [:!= id]))
      models/find-by
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
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/user (s/and (s/keys :req [:user/first-name :user/last-name :user/email]
                                    :opt [:user/password])
                            email-is-unique?))

(defmethod models/before-save :user
  [user]
  (update-in-if user [:user/password] hashers/derive))

(def ^:private sensitive-keys
  #{:user/password-reset-token
    :user/token-expires-at
    :user/password})

(defmethod models/after-read :user
  [user {:keys [include-password?]}]
  (if include-password?
    user
    (apply dissoc user sensitive-keys)))

(defn find-by-email
  "Returns the user having the specified email"
  [email]
  (models/find-by {:user/email email}))

(defn find-by-token
  "Returns the user having the specified, unexpired password reset token"
  [token]
  (models/find-by #:user{:password-reset-token token
                         :token-expires-at [:> (t/instant)]}
                  {:include-password? true}))

(defn authenticate
  "Returns the user with the specified username and password.
  The returned map contains the information cemerick friend
  needs to operate"
  [{:keys [username password]}]
  (when-let [user (models/find-by {:user/email username} {:include-password? true})]
    (when (hashers/check password (:user/password user))
      (-> user
          (dissoc :user/password)
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
        (assoc :user/password-reset-token token
               :user/token-expires-at (t/plus (t/instant) (t/hours 24)))
        models/put)
    token))

(defn reset-password
  "Changes the user's password to the specified value
  and invalidates the token"
  [token password]
  (if-let [user (find-by-token token)]
    (-> user
        (assoc :user/password password
               :user/password-reset-token nil
               :user/token-expires-at nil)
        models/put)
    (throw+ {:type ::models/not-found})))

(defn find-or-create-from-profile
  [{:keys [email]}]
  (models/find-by {:user/email email}))
