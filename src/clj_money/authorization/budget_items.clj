(ns clj-money.authorization.budget-items
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:budget-item ::authorization/manage]
  [budget-item action user]
  (owner-or-granted? budget-item user action))

(defmethod authorization/scope :budget-item
  [_ user action]
  (entity-scope :budget-item :budget/entity user action))
