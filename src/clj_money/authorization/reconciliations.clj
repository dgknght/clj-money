(ns clj-money.authorization.reconciliations
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:reconciliation ::authorization/manage]
  [reconciliation action user]
  (owner-or-granted? reconciliation user action))

(defmethod authorization/scope :reconciliation
  [_ user action]
  (entity-scope :reconciliation :account/entity user action))
