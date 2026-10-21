(ns clj-money.authorization.transaction-items
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:transaction-item ::authorization/manage]
  [item action user]
  (owner-or-granted? item user action))

(defmethod authorization/scope :transaction-item
  [_ user action]
  (entity-scope :transaction-item :transaction/entity user action))
