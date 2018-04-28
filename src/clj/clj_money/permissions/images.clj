(ns clj-money.permissions.images
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]))

(authorization/allow :image
                     (fn [user resource action _]
                       (and (= :show action)
                            (= (:user-id resource) (:id user)))))
