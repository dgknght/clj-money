(ns clj-money.authorization.lot-notes
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:lot-note ::authorization/manage]
  [entry action user]
  (owner-or-granted? entry user action))

(defmethod authorization/scope :lot-note
  [_ user action]
  (entity-scope :lot-note :account/entity user action))
