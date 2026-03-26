(ns clj-money.entities.invitations
  (:require [clojure.spec.alpha :as s]
            [dgknght.app-lib.core :refer [present?]]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities :as entities]))

(defn- recipient-is-not-a-user?
  [{:invitation/keys [recipient]}]
  (nil? (entities/find-by {:user/email recipient})))
(v/reg-spec recipient-is-not-a-user? {:message "%s is already in use"
                                      :path [:invitation/recipient]})

(s/def :invitation/recipient (s/and string?
                                    present?
                                    v/email?))
(s/def :invitation/note (s/nilable string?))
(s/def :invitation/status #{:unsent :sent :accepted :declined})
(s/def :invitation/user ::entities/entity-ref)

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/invitation
  (s/and (s/keys :req [:invitation/recipient
                        :invitation/status
                        :invitation/user]
                  :opt [:invitation/note])
         recipient-is-not-a-user?))
