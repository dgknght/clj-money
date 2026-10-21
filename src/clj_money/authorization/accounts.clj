(ns clj-money.authorization.accounts
  (:require [clj-money.authorization :as auth]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod auth/allowed? [:account ::auth/manage]
  [account action user]
  (owner-or-granted? account user action))

(defmethod auth/scope :account
  [_ user action]
  (entity-scope :account :account/entity user action))
