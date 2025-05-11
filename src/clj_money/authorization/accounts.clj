(ns clj-money.authorization.accounts
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :as util]
            [clj-money.authorization :as auth]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod auth/allowed? [:account ::auth/manage]
  [account action user]
  (owner-or-granted? account user action))

(defmethod auth/scope :account
  [_ user]
  (util/model-type {:entity/user user}
                 :account))
