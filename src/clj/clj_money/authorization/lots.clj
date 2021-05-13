(ns clj-money.authorization.lots
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/lot ::authorization/manage]
  [lot action user]
  (owner-or-granted? lot user action))

(defmethod authorization/scope ::models/lot
  [_ user]
  {[:commodity :entity :user-id] (:id user)})
