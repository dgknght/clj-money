(ns clj-money.authorization.transactions
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope :transaction
  [_ user]
  (util/model-type {:entity/user user} :transaction))

(defmethod authorization/scope :transaction-item
  [_ user]
  (util/model-type {:entity/user user} :transaction-item))
