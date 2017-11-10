(ns clj-money.permissions.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.coercion :as coercion]
            [clj-money.models.grants :as grants]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :entity user-owns-entity?)
(authorization/allow :entity (fn [user resource action context]
                               (and (:id resource)
                                    (user-granted-access? user resource action context))))
