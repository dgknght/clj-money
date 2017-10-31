(ns clj-money.permissions.reports
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.grants :as grants]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   find-grant]]))

(authorization/allow :report user-owns-entity?)
(authorization/allow :report (fn [user resource action context]
                               (grants/has-permission? (find-grant user resource context)
                                                       :report
                                                       (:type resource))))
