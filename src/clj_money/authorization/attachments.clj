(ns clj-money.authorization.attachments
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:attachment ::authorization/manage]
  [attachment action user]
  (owner-or-granted? attachment user action))

(defmethod authorization/scope :attachment
  [_ user action]
  (entity-scope :attachment :transaction/entity user action))
