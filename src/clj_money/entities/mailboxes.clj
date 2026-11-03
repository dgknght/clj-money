(ns clj-money.entities.mailboxes
  "A mailbox checked for emailed receipts (see clj-money.ingestion.mail).
  Each entity has at most one. The password is stored encrypted."
  (:require [clojure.spec.alpha :as s]
            [dgknght.app-lib.core :refer [update-in-if
                                          present?]]
            [clj-money.entities :as entities]))

(s/def :mailbox/entity ::entities/entity-ref)
(s/def :mailbox/host present?)
(s/def :mailbox/port (s/and int? pos?))
(s/def :mailbox/username present?)
(s/def :mailbox/password present?)
(s/def :mailbox/folder present?)
(s/def :mailbox/allowed-senders (s/nilable (s/coll-of string? :kind sequential?)))
(s/def :mailbox/uid-validity (s/nilable int?))
(s/def :mailbox/last-uid (s/nilable int?))
(s/def :mailbox/checked-at (s/nilable inst?))
(s/def :mailbox/error (s/nilable string?))

(s/def ::entities/mailbox (s/keys :req [:mailbox/entity
                                        :mailbox/host
                                        :mailbox/port
                                        :mailbox/username
                                        :mailbox/password
                                        :mailbox/folder]
                                  :opt [:mailbox/allowed-senders
                                        :mailbox/uid-validity
                                        :mailbox/last-uid
                                        :mailbox/checked-at
                                        :mailbox/error]))

; stored as edn, so the list keeps its shape in either storage
(defmethod entities/before-save :mailbox
  [mailbox]
  (update-in-if mailbox [:mailbox/allowed-senders] pr-str))

(defmethod entities/after-read :mailbox
  [mailbox _]
  (update-in-if mailbox [:mailbox/allowed-senders] read-string))
