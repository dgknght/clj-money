(ns clj-money.authorization.transactions
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope :transaction
  [_ user]
  {:entity/user user})

(defmethod authorization/scope :transaction-item
  [_ user]
  {:entity/user user})
