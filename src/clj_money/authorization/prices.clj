(ns clj-money.authorization.prices
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:price ::authorization/manage]
  [price action user]
  (owner-or-granted? price user action))

(defmethod authorization/scope :price
  [_ user]
  {:entity/user user})
