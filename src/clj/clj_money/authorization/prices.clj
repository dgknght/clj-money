(ns clj-money.authorization.prices
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/price ::authorization/manage]
  [price action user]
  (owner-or-granted? price user action))

(defmethod authorization/scope ::models/price
  [_ user]
  {[:commodity :entity :user-id] (:id user)})
