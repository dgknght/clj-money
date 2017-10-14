(ns clj-money.permissions.commodities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-entity-ids
                                                   user-owns-entity?]]))

(authorization/allow :commodity user-owns-entity?)

(authorization/set-scope
  :commodity
  {:entity-id user-entity-ids})
