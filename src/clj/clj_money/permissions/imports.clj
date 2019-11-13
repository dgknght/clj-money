(ns clj-money.permissions.imports
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]))

(authorization/allow :import (fn [user imp & _] (= (:id user)  (:user-id imp))))

(authorization/set-scope :import
                         {:user-id (fn [user _] (:id user))})
