(ns clj-money.authorization.transactions
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope :transaction
  [_ user action]
  (entity-scope :transaction :transaction/entity user action))
