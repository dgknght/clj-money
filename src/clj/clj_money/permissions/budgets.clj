(ns clj-money.permissions.budgets
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [all-user-entity-ids
                                                   user-granted-access?
                                                   user-owns-entity?]]))

(authorization/set-scope
  :budget
  {:entity-id all-user-entity-ids})

(authorization/allow :budget user-owns-entity?)
(authorization/allow :budget user-granted-access?)

(authorization/allow :budget-item user-owns-entity?)
(authorization/allow :budget-item user-granted-access?)
