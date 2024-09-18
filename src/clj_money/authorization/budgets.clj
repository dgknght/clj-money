(ns clj-money.authorization.budgets
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/budget ::authorization/manage]
  [budget action user]
  (owner-or-granted? budget user action))

(defmethod authorization/allowed? [::models/budget-item ::authorization/manage]
  [budget-item action user]
  (owner-or-granted? budget-item user action))

(defmethod authorization/scope ::models/budget
  [_ user]
  {[:entity :user-id] (:id user)})
