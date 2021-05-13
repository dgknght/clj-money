(ns clj-money.authorization.scheduled-transactions
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(derive ::realize ::authorization/manage)

(defmethod authorization/allowed? [::models/scheduled-transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope ::models/scheduled-transaction
  [_ user]
  {[:entity :user-id] (:id user)})
