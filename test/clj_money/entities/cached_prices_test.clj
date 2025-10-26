(ns clj-money.entities.cached-prices-test
  (:require [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.entity-helpers :refer [assert-created
                                             assert-invalid]]
            [clj-money.core]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context]]
            [clj-money.test-helpers :refer [dbtest]]))

(def attributes
  #:cached-price{:trade-date (t/local-date 2017 3 2)
                 :symbol "TESLA"
                 :exchange :nasdaq
                 :value 100M})

(dbtest create-a-cached-price
  (assert-created attributes))

(dbtest symbol-is-required
  (assert-invalid (dissoc attributes :cached-price/symbol)
                  {:cached-price/symbol ["Symbol is required"]}))

(dbtest trade-date-is-required
  (assert-invalid (dissoc attributes :cached-price/trade-date)
                  {:cached-price/trade-date ["Trade date is required"]}))

(dbtest exchange-is-required
  (assert-invalid (dissoc attributes :cached-price/exchange)
                  {:cached-price/exchange ["Exchange is required"]}))

(dbtest value-is-required
  (assert-invalid (dissoc attributes :cached-price/value)
                  {:cached-price/value ["Value is required"]}))

(def ^:private existing-context
  [attributes])

(dbtest trade-date-must-be-unique
  (with-context existing-context
    (assert-invalid attributes
                    {:cached-price/trade-date ["Trade date already exists"]})))
