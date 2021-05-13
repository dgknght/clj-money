(ns clj-money.authorization.transactions
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope ::models/transaction
  [_ user]
  {[:entity :user-id] (:id user)})

(defmethod authorization/scope ::models/transaction-item
  [_ user]
  {[:transaction :entity :user-id] (:id user)})
