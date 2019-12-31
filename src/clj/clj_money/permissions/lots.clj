(ns clj-money.permissions.lots
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [all-user-entity-ids
                                                   user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :lot user-owns-entity?)
(authorization/allow :lot user-granted-access?)

(authorization/set-scope
  :lot
  {:entity-id all-user-entity-ids})
