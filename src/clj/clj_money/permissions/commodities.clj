(ns clj-money.permissions.commodities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [all-user-entity-ids
                                                   user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :commodity user-owns-entity?)
(authorization/allow :commodity user-granted-access?)

(authorization/set-scope
  :commodity
  {:entity-id all-user-entity-ids})
