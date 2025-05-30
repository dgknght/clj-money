(ns clj-money.models.cached-prices-test
  (:require [clojure.test :refer [deftest use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.model-helpers :refer [assert-created
                                             assert-invalid]]
            [clj-money.core]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def attributes
  #:cached-price{:trade-date (t/local-date 2017 3 2)
                 :symbol "TESLA"
                 :exchange :nasdaq
                 :value 100M})

(deftest create-a-cached-price
  (assert-created attributes))

(deftest symbol-is-required
  (assert-invalid (dissoc attributes :cached-price/symbol)
                  {:cached-price/symbol ["Symbol is required"]}))

(deftest trade-date-is-required
  (assert-invalid (dissoc attributes :cached-price/trade-date)
                  {:cached-price/trade-date ["Trade date is required"]}))

(deftest exchange-is-required
  (assert-invalid (dissoc attributes :cached-price/exchange)
                  {:cached-price/exchange ["Exchange is required"]}))

(deftest value-is-required
  (assert-invalid (dissoc attributes :cached-price/value)
                  {:cached-price/value ["Value is required"]}))

(def ^:private existing-context
  [attributes])

(deftest trade-date-must-be-unique
  (with-context existing-context
    (assert-invalid attributes
                    {:cached-price/trade-date ["Trade date already exists"]})))
