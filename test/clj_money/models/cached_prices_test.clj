(ns clj-money.models.cached-prices-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.core]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.cached-prices :as cached-prices]))

(use-fixtures :each reset-db)

(def attributes
  {:trade-date (t/local-date 2017 3 2)
   :symbol "TESLA"
   :exchange :nasdaq
   :price 100M})

(deftest create-a-cached-price
  (let [cached-price (cached-prices/create attributes)
        retrieved (cached-prices/find cached-price)]
    (is (:id cached-price) "An :id is assigned to the record")
    (is (valid? cached-price))
    (is (comparable? attributes cached-price) "The correct attributes are returned")
    (is (comparable? attributes retrieved) "The correct attributes are retrieved")))

(deftest symbol-is-required
  (let [cached-price (cached-prices/create (dissoc attributes :symbol))
        retrieved (cached-prices/find-by attributes)]
    (is (nil? (:id cached-price))
        "The result does not contain an ID value")
    (is (invalid? cached-price [:symbol] "Symbol is required"))
    (is (nil? retrieved) "The record is not created")))

(deftest trade-date-is-required
  (let [cached-price (cached-prices/create (dissoc attributes :trade-date))
        retrieved (cached-prices/find-by attributes)]
    (is (nil? (:id cached-price))
        "The result does not contain an ID value")
    (is (invalid? cached-price [:trade-date] "Trade date is required"))
    (is (nil? retrieved) "The record is not created")))

(deftest exchange-is-required
  (let [cached-price (cached-prices/create (dissoc attributes :exchange))
        retrieved (cached-prices/find-by attributes)]
    (is (nil? (:id cached-price))
        "The result does not contain an ID value")
    (is (invalid? cached-price [:exchange] "Exchange is required"))
    (is (nil? retrieved) "The record is not created")))

(deftest price-is-required
  (let [cached-price (cached-prices/create (dissoc attributes :price))
        retrieved (cached-prices/find-by attributes)]
    (is (nil? (:id cached-price))
        "The result does not contain an ID value")
    (is (invalid? cached-price [:price] "Price is required"))
    (is (nil? retrieved) "The record is not created")))

(def ^:private existing-context
  {:cached-prices [attributes]})

(deftest trade-date-must-be-unique
  (with-context existing-context
  (let [cached-price (cached-prices/create attributes)
        retrieved (cached-prices/search (select-keys attributes [:trade-date :symbol :exchange]))]
    (is (nil? (:id cached-price))
        "The duplicate value does not receive an ID")
    (is (invalid? cached-price [:trade-date] "Trade date already exists"))
    (is (= 1 (count retrieved))
        "The the duplicate cached-price is not saved"))))
