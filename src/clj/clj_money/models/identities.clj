(ns clj-money.models.identities
  (:refer-clojure :exclude [find])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [clj-money.models.storage :refer [create-identity
                                              select-identities]]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn]]
            [clj-money.models.users :as users]))

(s/def ::user-id integer?)
(s/def ::provider #{:google})
(s/def ::provider-id string?)
(s/def ::identity (s/keys :req-un [::user-id ::provider ::provider-id]))

(defn- before-save
  [ident _]
  ident)

(def create
  (create-fn {:spec ::identity
              :create #(create-identity %2 %1)
              :before-save before-save}))

(defn select
  [storage-spec criteria options]
  (with-storage [s storage-spec]
    (select-identities s criteria options)))

(defn find
  [storage-spec criteria]
  (first (select storage-spec criteria {:limit 1})))

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
  [storage provider {:keys [id]}]
  (identity->user (find storage
                        {:provider provider
                         :provider-id id})))

(defn- find-by-email
  [storage provider {:keys [email id]}]
  (when-let [user (users/find storage {:email email})]
    (create storage {:provider provider
                     :provider-id id
                     :user-id (:id user)})
    user))

(defn- create-from-profile
  [storage provider {:keys [email id given_name family_name]}]
  (let [user (users/create storage {:email email
                                    :first-name given_name
                                    :last-name family_name
                                    :password "please001!"
                                    :password-confirmation "please001!"})
        ident (create storage {:provider provider
                     :provider-id id
                     :user-id (:id user)})]
    (log/debugf "created user from profile %s" (prn-str user))
    (log/debugf "created identity from profile %s" (prn-str ident))
    user))

(defn find-or-create-from-profile
  [storage-spec provider profile]
  (with-storage [s storage-spec]
    (some #(% s provider profile)
          [find-by-identity
           find-by-email
           create-from-profile])))
