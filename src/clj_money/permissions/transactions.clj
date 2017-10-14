(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-entity-ids]]))

(authorization/allow :transaction user-owns-entity?)

(authorization/set-scope :transaction {:entity-id user-entity-ids})
