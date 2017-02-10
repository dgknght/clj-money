(ns clj-money.models.prices-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private price-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}
              {:name "Business"}]
   :commodities [{:name "Apple"
                  :symbol "APPL"
                  :exchange :nasdaq}]})

(deftest a-price-can-be-addied-for-a-commodity
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (integer? (:id price))
        "The result contains an ID value")
    (is (empty? (validation/error-messages price))
        "The result does not contain any validation errors")
    (is (seq (filter #(= (t/local-date 2017 3 2) (:trade-date %)) prices))
        "The price can be retrieved after create")))

(deftest price-commodity-id-is-required
  (is false "need to write the test"))

(deftest price-trade-date-is-required
  (is false "need to write the test"))

(deftest price-trace-date-must-be-a-date
  (is false "need to write the test"))

(deftest price-trace-date-must-be-a-string-date
  (is false "need to write the test"))

(deftest price-value-is-required
  (is false "need to write the test"))

(deftest price-value-must-be-a-number
  (is false "need to write the test"))

(deftest price-value-can-be-a-string-number
  (is false "need to write the test"))
