(ns clj-money.authorization.transaction-items
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:transaction-item ::authorization/manage]
  [item action user]
  (owner-or-granted? item user action))

(defmethod authorization/scope :transaction-item
  [_ user]
  (util/entity-type {:entity/user user}
                    :transaction-item))
