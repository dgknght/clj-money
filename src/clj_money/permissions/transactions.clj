(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-entity-ids
                                                   user-granted-access?]]))

(authorization/allow :transaction user-owns-entity?)
(authorization/allow :transaction user-granted-access?)

(authorization/set-scope :transaction {:entity-id user-entity-ids})
