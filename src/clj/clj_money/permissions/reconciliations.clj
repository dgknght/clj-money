(ns clj-money.permissions.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :reconciliation user-owns-entity?)
(authorization/allow :reconciliation user-granted-access?)
