(ns clj-money.authorization.budgets
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:budget ::authorization/manage]
  [budget action user]
  (owner-or-granted? budget user action))

(defmethod authorization/scope :budget
  [_ user]
  (util/model-type {:entity/user user}
                 :budget))
