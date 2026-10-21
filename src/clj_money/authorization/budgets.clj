(ns clj-money.authorization.budgets
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:budget ::authorization/manage]
  [budget action user]
  (owner-or-granted? budget user action))

(defmethod authorization/scope :budget
  [_ user action]
  (entity-scope :budget :budget/entity user action))
