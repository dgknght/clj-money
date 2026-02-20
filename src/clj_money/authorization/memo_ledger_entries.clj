(ns clj-money.authorization.memo-ledger-entries
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:memo-ledger-entry ::authorization/manage]
  [entry action user]
  (owner-or-granted? entry user action))

(defmethod authorization/scope :memo-ledger-entry
  [_ user]
  {:entity/user user})
