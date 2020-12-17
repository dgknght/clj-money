(ns clj-money.authorization.accounts
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/account ::authorization/manage]
  [account action user]
  (owner-or-granted? account user action))

(defmethod authorization/scope ::models/account
  [_ user]
  {[:entity :user-id] (:id user)})
