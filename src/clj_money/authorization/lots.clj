(ns clj-money.authorization.lots
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:lot ::authorization/manage]
  [lot action user]
  (owner-or-granted? lot user action))

(defmethod authorization/scope :lot
  [_ user action]
  (entity-scope :lot :account/entity user action))
