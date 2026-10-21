(ns clj-money.authorization.scheduled-transactions
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(derive ::realize ::authorization/manage)

(defmethod authorization/allowed? [:scheduled-transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope :scheduled-transaction
  [_ user action]
  (entity-scope :scheduled-transaction :scheduled-transaction/entity user action))
