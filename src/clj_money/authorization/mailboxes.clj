(ns clj-money.authorization.mailboxes
  "A mailbox holds the credentials to an email account, so only the owner
  of the entity can manage it. No grant gives access to it."
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:mailbox ::authorization/manage]
  [mailbox action user]
  (owner-or-granted? mailbox user action))

(defmethod authorization/scope :mailbox
  [_ user action]
  (entity-scope :mailbox :mailbox/entity user action))
