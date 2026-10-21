(ns clj-money.authorization.prices
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:price ::authorization/manage]
  [price action user]
  (owner-or-granted? price user action))

(defmethod authorization/scope :price
  [_ user action]
  (entity-scope :price :commodity/entity user action))
