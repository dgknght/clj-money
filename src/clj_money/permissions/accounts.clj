(ns clj-money.permissions.accounts
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [all-user-entity-ids
                                                   user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :account user-owns-entity?)
(authorization/allow :account user-granted-access?)

(authorization/set-scope
  :account
  {:entity-id all-user-entity-ids})
