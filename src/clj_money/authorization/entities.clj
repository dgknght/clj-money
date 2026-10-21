(ns clj-money.authorization.entities
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:entity ::authorization/manage]
  [entity action user]
  (owner-or-granted? entity user action))

(defmethod authorization/scope :entity
  [_ user action]
  (entity-scope :entity :id user action))
