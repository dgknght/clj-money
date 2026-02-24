(ns clj-money.authorization.lot-notes
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:lot-note ::authorization/manage]
  [entry action user]
  (owner-or-granted? entry user action))

(defmethod authorization/scope :lot-note
  [_ user]
  (util/entity-type
    {:entity/user user}
    :lot-note))
