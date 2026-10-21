(ns clj-money.authorization.commodities
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:commodity ::authorization/manage]
  [commodity action user]
  (owner-or-granted? commodity user action))

(defmethod authorization/scope :commodity
  [_ user action]
  (entity-scope :commodity :commodity/entity user action))
