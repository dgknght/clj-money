(ns clj-money.permissions.lots
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :lot user-owns-entity?)
(authorization/allow :lot user-granted-access?)

(authorization/set-scope
  :lot
  {[:commodity :entity :user-id] (fn [user & _]
                                   (:id user))})
