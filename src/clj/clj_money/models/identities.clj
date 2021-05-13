(ns clj-money.models.identities
  (:refer-clojure :exclude [find])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.users :as users]))

(s/def ::user-id integer?)
(s/def ::provider #{:google})
(s/def ::provider-id string?)
(s/def ::identity (s/keys :req-un [::user-id ::provider ::provider-id]))

(defn- before-save
  [ident]
  (tag ident ::models/identity))

(defn- after-read
  [ident]
  (tag ident ::models/identity))

(defn create
  [ident]
  (with-storage (env :db)
    (with-validation ident ::identity
      (-> ident
          before-save
          storage/create
          after-read))))

(defn select
  [criteria options]
  (with-storage (env :db)
    (storage/select (tag criteria ::models/identity)
                    options)))

(defn find-by
  [criteria]
  (first (select criteria {:limit 1})))

(defn- identity->user
  [ident]
  (when ident
    (-> ident
        (rename-keys {:user-id :id
                      :user-first-name :first-name
                      :user-last-name :last-name
                      :user-email :email})
        (dissoc :provider :provider-id))))

(defn- find-by-identity
  [provider {:keys [id]}]
  (identity->user (find-by
                   {:provider provider
                    :provider-id id})))

(defn- find-by-email
  [provider {:keys [email id]}]
  (when-let [user (users/find-by {:email email})]
    (create {:provider provider
             :provider-id id
             :user-id (:id user)})
    user))

(defn- create-from-profile
  [provider {:keys [email id given_name family_name]}]
  (let [user (users/create {:email email
                            :first-name given_name
                            :last-name family_name
                            :password "please001!"
                            :password-confirmation "please001!"})
        ident (create {:provider provider
                       :provider-id id
                       :user-id (:id user)})]
    (log/debugf "created user from profile %s" (prn-str user))
    (log/debugf "created identity from profile %s" (prn-str ident))
    user))

(defn find-or-create-from-profile
  [provider profile]
  (with-storage (env :db)
    (some #(% provider profile)
          [find-by-identity
           find-by-email
           create-from-profile])))
