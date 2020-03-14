(ns clj-money.authorization.commodities
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/commodity ::authorization/manage]
  [commodity action user]
  (owner-or-granted? commodity user action))

(defmethod authorization/scope ::models/commodity
  [_ user]
  {[:entity :user-id] (:id user)})
