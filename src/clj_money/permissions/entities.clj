(ns clj-money.permissions.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.coercion :as coercion]
            [clj-money.models.grants :as grants]))

(authorization/allow :entity
                     (fn [user resource _ _]
                       (= (:id user) (:user-id resource))))
(authorization/allow :entity
                     (fn [user resource action {storage-spec :storage-spec}]
                       ; :show permission is implicit on entity with the grant
                       (and (:id resource)
                            (seq (grants/search storage-spec {:user-id (:id user)
                                                              :entity-id (:id resource)})))))
