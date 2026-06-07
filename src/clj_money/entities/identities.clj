(ns clj-money.entities.identities
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clj-money.entities :as entities]))

(s/def :identity/user ::entities/entity-ref)
(s/def :identity/provider #{:google :github})
(s/def :identity/provider-id string?)
(s/def ::entities/identity (s/keys :req [:identity/user
                                       :identity/provider
                                       :identity/provider-id]))

(defn- find-by-identity
  [[provider {:keys [id profile_photo]}]]
  (some-> #:identity{:provider provider
                     :provider-id id}
          entities/find-by
          :identity/user
          entities/find
          (assoc :user/profile-photo profile_photo)
          entities/put))

(defn- find-by-email
  [[provider {:keys [email id profile_photo]}]]
  (when-let [user (entities/find-by {:user/email email})]
    (entities/put #:identity{:provider provider
                             :provider-id id
                             :user user})
    (entities/put (assoc user :user/profile-photo profile_photo))))

(defn- create-from-profile
  [[provider {:keys [email id given_name family_name profile_photo]}]]
  (let [user (entities/put #:user{:email email
                                  :first-name given_name
                                  :last-name family_name
                                  :password "please001!"
                                  :profile-photo profile_photo
                                  :roles #{:user}})
        ident (entities/put #:identity{:provider provider
                                       :provider-id id
                                       :user user})]
    (log/debugf "created user from profile %s"
                (prn-str (entities/scrub-sensitive-data user)))
    (log/debugf "created identity from profile %s"
                (prn-str (entities/scrub-sensitive-data ident)))
    user))

(def find-or-create-from-profile
  (some-fn find-by-identity
           find-by-email
           create-from-profile))
