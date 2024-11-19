(ns clj-money.models.identities
  (:refer-clojure :exclude [find])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.models :as models]
            [clj-money.models.users :as users]))

(s/def :identity/user ::models/model-ref)
(s/def :identity/provider #{:google})
(s/def :identity/provider-id string?)
(s/def ::models/identity (s/keys :req [:identity/user
                                       :identity/provider
                                       :identity/provider-id]))

(defn ^:deprecated create
  [_ident]
  (throw (UnsupportedOperationException. "create is deprecated")))

(defn ^:deprecated select
  [_criteria _options]
  (throw (UnsupportedOperationException. "select is deprecated")))

(defn find-by
  [_criteria]
  (throw (UnsupportedOperationException. "find-by is deprecated")))

(defn- identity->user
  [ident]
  (when ident
    (-> ident
        (rename-keys {:user-id :id
                      :user-first-name :user/first-name
                      :user-last-name :user/last-name
                      :user-email :user/email})
        (dissoc :provider :provider-id))))

(defn- find-by-identity
  [provider {:keys [id]}]
  (identity->user (models/find-by
                    #:identity{:provider provider
                               :provider-id id})))

(defn- find-by-email
  [provider {:keys [email id]}]
  (when-let [user (models/find-by {:user/email email})]
    (models/put #:identity{:provider provider
                           :provider-id id
                           :user user})
    user))

(defn- create-from-profile
  [provider {:keys [email id given_name family_name]}]
  (let [user (models/put #:user{:email email
                                :first-name given_name
                                :last-name family_name
                                :password "please001!"
                                :password-confirmation "please001!"})
        ident (models/put #:identity{:provider provider
                                     :provider-id id
                                     :user user})]
    (log/debugf "created user from profile %s" (prn-str user))
    (log/debugf "created identity from profile %s" (prn-str ident))
    user))

(defn find-or-create-from-profile
  [provider profile]
  (some #(% provider profile)
        [find-by-identity
         find-by-email
         create-from-profile]))
