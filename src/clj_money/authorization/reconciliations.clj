(ns clj-money.authorization.reconciliations
  (:require [clj-money.authorization :as authorization]
            [clj-money.util :as util]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:reconciliation ::authorization/manage]
  [reconciliation action user]
  (owner-or-granted? reconciliation user action))

(defmethod authorization/scope :reconciliation
  [_ user]
  (util/entity-type
    {:entity/user user}
    :reconciliation))
