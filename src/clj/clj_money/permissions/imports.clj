(ns clj-money.permissions.imports
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [all-user-entity-ids
                                                   user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :import (fn [user imp & _] (= (:id user)  (:user-id imp))))

(authorization/set-scope :import
                         {:user-id (fn [user _] (:id user))})
