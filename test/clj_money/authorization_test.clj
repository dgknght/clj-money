(ns clj-money.authorization-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [slingshot.test :refer :all]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.authorization :refer [allowed?
                                             tag-resource
                                             apply-scope]]
            [clj-money.permissions.entities]
            [clj-money.permissions.grants]
            [clj-money.permissions.accounts]
            [clj-money.permissions.transactions]
            [clj-money.permissions.commodities]
            [clj-money.permissions.prices]
            [clj-money.permissions.budgets]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-account
                                            find-users
                                            find-entity
                                            find-grant
                                            find-budget
                                            find-commodity
                                            find-price]]))
