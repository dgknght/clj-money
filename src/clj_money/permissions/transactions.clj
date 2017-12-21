(ns clj-money.permissions.transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   all-user-entity-ids
                                                   user-granted-access?]]))

(authorization/allow :transaction user-owns-entity?)
(authorization/allow :transaction user-granted-access?)

(authorization/set-scope :transaction {:entity-id all-user-entity-ids})
