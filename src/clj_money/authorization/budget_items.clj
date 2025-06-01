(ns clj-money.authorization.budget-items
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:budget-item ::authorization/manage]
  [budget-item action user]
  (owner-or-granted? budget-item user action))

(defmethod authorization/scope :budget-item
  [_ user]
  (util/model-type {:entity/user user}
                   :budget-item))
