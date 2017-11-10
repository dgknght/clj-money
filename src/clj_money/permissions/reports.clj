(ns clj-money.permissions.reports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.grants :as grants]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   find-grant]]))

(authorization/allow :report user-owns-entity?)
(authorization/allow :report (fn [user resource action context]
                               (let [grant (find-grant user resource context)]
                                 (grants/has-permission?
                                   grant
                                   :report
                                   (:type resource)))))
