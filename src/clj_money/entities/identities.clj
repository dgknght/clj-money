(ns clj-money.entities.identities
  (:refer-clojure :exclude [find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clj-money.entities :as models]))

(s/def :identity/user ::models/model-ref)
(s/def :identity/provider #{:google})
(s/def :identity/provider-id string?)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/identity (s/keys :req [:identity/user
                                       :identity/provider
                                       :identity/provider-id]))

(defn- find-by-identity
  [[provider {:keys [id]}]]
  (when-let [ident (models/find-by
                     #:identity{:provider provider
                                :provider-id id})]
    (models/find (:identity/user ident)
                 :user)))

(defn- find-by-email
  [[provider {:keys [email id]}]]
  (when-let [user (models/find-by {:user/email email})]
    (models/put #:identity{:provider provider
                           :provider-id id
                           :user user})
    user))

(defn- create-from-profile
  [[provider {:keys [email id given_name family_name]}]]
  (let [user (models/put #:user{:email email
                                :first-name given_name
                                :last-name family_name
                                :password "please001!"})
        ident (models/put #:identity{:provider provider
                                     :provider-id id
                                     :user user})]
    (log/debugf "created user from profile %s" (prn-str user))
    (log/debugf "created identity from profile %s" (prn-str ident))
    user))

(def find-or-create-from-profile
  (some-fn find-by-identity
           find-by-email
           create-from-profile))
