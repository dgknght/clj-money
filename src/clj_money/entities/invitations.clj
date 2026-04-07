(ns clj-money.entities.invitations
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [dgknght.app-lib.core :refer [present?]]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities :as entities]))

(defn- recipient-is-not-a-user?
  [{:keys [id] :invitation/keys [recipient]}]
  (or id
      (zero? (entities/count {:user/email recipient}))))
(v/reg-spec recipient-is-not-a-user? {:message "%s is already in use"
                                      :path [:invitation/recipient]})

(s/def :invitation/recipient (s/and string?
                                    present?
                                    v/email?))
(s/def :invitation/note (s/nilable string?))
(s/def :invitation/status #{:unsent :sent :accepted :declined})
(s/def :invitation/invited-by ::entities/entity-ref)
(s/def :invitation/user (s/nilable ::entities/entity-ref))
(s/def :invitation/token (s/and string? present?))
(s/def :invitation/expires-at inst?)

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/invitation
  (s/and (s/keys :req [:invitation/recipient
                        :invitation/status
                        :invitation/invited-by
                        :invitation/token
                        :invitation/expires-at]
                  :opt [:invitation/note
                        :invitation/user])
         recipient-is-not-a-user?))

(defn generate-token []
  (string/replace (random-uuid) "-" ""))
