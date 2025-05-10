(ns clj-money.authorization.scheduled-transactions
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(derive ::realize ::authorization/manage)

(defmethod authorization/allowed? [:scheduled-transaction ::authorization/manage]
  [transaction action user]
  (owner-or-granted? transaction user action))

(defmethod authorization/scope :scheduled-transaction
  [_ user]
  (util/model-type {:entity/user user} :scheduled-transaction))
