(ns clj-money.permissions.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.coercion :as coercion]))

(authorization/allow :entity
       (fn [user resource _ _]
         (= (:id user) (:user-id resource))))
