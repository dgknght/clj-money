(ns clj-money.permissions.accounts
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :trade user-owns-entity?)
(authorization/allow :trade user-granted-access?)
