(ns clj-money.authorization.lot-notes
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:lot-note ::authorization/manage]
  [entry action user]
  (owner-or-granted? entry user action))

(defmethod authorization/scope :lot-note
  [_ user]
  {:entity/user user})
