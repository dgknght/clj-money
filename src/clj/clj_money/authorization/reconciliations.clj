(ns clj-money.authorization.reconciliations
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/reconciliation ::authorization/manage]
  [reconciliation action user]
  (owner-or-granted? reconciliation user action))

(defmethod authorization/scope ::models/reconciliation
  [_ user]
  {[:account :entity :user-id] (:id user)})
