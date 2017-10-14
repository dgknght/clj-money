(ns clj-money.permissions.budgets
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-entity-ids
                                                   user-owns-entity?]]
            [clj-money.models.budgets :as budgets]))

(authorization/set-scope
  :budget
  {:entity-id user-entity-ids})

(authorization/allow :budget user-owns-entity?)

(authorization/allow :budget-item
                     (fn [user resource _ {storage-spec :storage-spec :as context}]
                       (user-owns-entity? user (budgets/find-by-id storage-spec (:budget-id resource)) context)))
