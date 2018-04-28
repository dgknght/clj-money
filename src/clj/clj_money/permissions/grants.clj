(ns clj-money.permissions.grants
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-entity-ids
                                                   user-owns-entity?]]))

(authorization/allow :grant user-owns-entity?)

(authorization/set-scope
  :grant
  {:entity-id user-entity-ids})
