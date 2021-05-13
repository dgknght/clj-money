(ns clj-money.models.prices-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.core]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(use-fixtures :each reset-db)

(def ^:private price-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}
              {:name "Business"}]
   :commodities [{:name "Apple"
                  :symbol "AAPL"
                  :type :stock
                  :exchange :nasdaq
                  :entity-id "Personal"}
                 {:name "US Dollar"
                  :symbol "USD"
                  :type :currency
                  :entity-id "Personal"}]})

(defn- attributes
  [commodity]
  {:trade-date (t/local-date 2017 3 2)
   :price 12.34M
   :commodity-id (:id commodity)})

(deftest create-a-price
  (let [context (realize price-context)
        commodity (find-commodity context "AAPL")
        attr (attributes commodity)
        price (prices/create attr)
        retrieved (prices/find price)
        updated-commodity (commodities/find commodity)]
    (is (:id price)
        "The result contains an ID value")
    (is (valid? price))
    (is (comparable? attr price) "The correct attributes are returned")
    (is (comparable? attr retrieved) "The correct attributes are retrieved")
    (is (= (t/local-date 2017 3 2)
           (:earliest-price updated-commodity))
        "The commodity earliest-price is updated")
    (is (= (t/local-date 2017 3 2)
           (:latest-price updated-commodity))
        "The commodity latest-price is updated")))

(deftest commodity-id-is-required
  (let [context (realize price-context)
        entity (find-entity context "Personal")
        attr (dissoc (attributes {}) :commodity-id)
        price (prices/create attr)
        retrieved (prices/find-by (assoc attr [:commodity :entity-id] (:id entity)))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (invalid? price [:commodity-id] "Commodity is required"))
    (is (nil? retrieved) "The record is not created")))

(deftest trade-date-is-required
  (let [context (realize price-context)
        commodity (find-commodity context "AAPL")
        attr (dissoc (attributes commodity) :trade-date)
        price (prices/create attr)
        retrieved(prices/find-by (assoc attr :trade-date [:between
                                                          (t/local-date 2008 1 1)
                                                          (t/local-date 2017 12 13)]))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (invalid? price [:trade-date] "Trade date is required"))
    (is (nil? retrieved)
        "The price cannot be retrieved after create")))

(deftest trade-date-must-be-a-date
  (let [context (realize price-context)
        commodity (find-commodity context "AAPL")
        attr (assoc (attributes commodity) :trade-date "notadate")
        price (prices/create attr)
        retrieved (prices/find-by (assoc attr :trade-date [:between
                                                          (t/local-date 2008 1 1)
                                                          (t/local-date 2017 12 13)]))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (invalid? price [:trade-date] "Trade date must be a date"))
    (is (nil? retrieved)
        "The price cannot be retrieved after create")))

(def ^:private existing-price-context
  (assoc price-context :prices [{:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 3 2)
                                 :price 12.34M}]))

(deftest trade-date-must-be-unique
  (let [context (realize existing-price-context)
        commodity (find-commodity context "AAPL")
        attr (attributes commodity)
        price (prices/create attr)
        retrieved (prices/search attr)]
    (is (nil? (:id price))
        "The duplicate value does not receive an ID")
    (is (invalid? price [:trade-date] "Trade date already exists"))
    (is (= 1 (count retrieved))
        "The the duplicate price is not saved")))

(deftest price-is-required
  (let [context (realize price-context)
        commodity (find-commodity context "AAPL")
        attr (dissoc (attributes commodity) :price)
        price (prices/create attr)
        retrieved (prices/find-by attr)]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (invalid? price [:price] "Price is required")
        "The result contains a validation error")
    (is (nil? retrieved)
        "The price cannot be retrieved after create")))

(deftest price-must-be-a-number
  (let [context (realize price-context)
        commodity (find-commodity context "AAPL")
        attr (assoc (attributes commodity) :price "notanumber")
        price (prices/create attr)
        retrieved (prices/find-by (dissoc attr :price))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (invalid? price [:price] "Price must be a number"))
    (is (nil? retrieved)
        "The price cannot be retrieved after create")))

(deftest a-price-can-be-updated
  (let [context (realize existing-price-context)
        price (-> context :prices first)
        result (prices/update (assoc price :price 10M))
        retrieved (prices/find price)]
    (is (valid? result))
    (is (= 10.00M (:price retrieved))
        "The retrieved map has the correct values")))

(deftest a-price-can-be-deleted
  (let [context (realize existing-price-context)
        price (-> context :prices first)
        _ (prices/delete price)
        retrieved (prices/find-by {:commodity-id (:commodity-id price)
                               :trade-date (:trade-date price)})]
    (is (nil? retrieved) "The result is not retrieved after delete")))

(def ^:private multi-price-context
  (assoc price-context :prices [{:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 2 27)
                                 :price 12.34M}
                                {:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 3 2)
                                 :price 12.20M}
                                {:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 3 1)
                                 :price 12.00M}]))

(deftest get-the-most-recent-price-for-a-commodity
  (let [context (realize multi-price-context)]
    (testing "When at least one price exists"
      (let [commodity (find-commodity context "AAPL")
            price (prices/most-recent (commodities/find commodity))]
        (is (= 12.20M (:price price)) "The most recent price is returned")))
    (testing "When no prices exist"
      (let [commodity (find-commodity context "USD")
            price (prices/most-recent commodity)]
        (is (nil? price) "The nil is returned")))))

(deftest deleting-a-commodity-deletes-the-prices
  (let [context (realize existing-price-context)
        commodity (-> context :commodities first)
        criteria {:commodity-id (:id commodity)
                  :trade-date [:between
                               (t/local-date 2016 1 1)
                               (t/local-date 2017 12 31)]}
        prices-before (prices/search criteria)
        _ (commodities/delete commodity)
        prices-after (prices/search criteria)]
    (is (seq prices-before)
        "The commodity prices exist before delete")
    (is (empty? prices-after)
        "The commodity prices are absent after delete")))
