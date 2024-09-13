(ns clj-money.models.prices-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [dgknght.app-lib.validation :as v]
            [clj-money.core]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            realize
                                            find-entity
                                            find-commodity
                                            find-price]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.entities :as entities]
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
        "The commodity latest-price is updated")
    (testing "entity price date boundaries are set"
      (let [entity (entities/find (:entity-id commodity))]
        (is (comparable? {:earliest-price-date (t/local-date 2017 3 2)
                          :latest-price-date (t/local-date 2017 3 2)}
                         (:settings entity))
            "The price date boundaries are updated in the entity")))))

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

(deftest update-a-price
  (let [context (realize existing-price-context)
        price (-> context :prices first)
        result (prices/update (assoc price :price 10M))
        retrieved (prices/find price)]
    (is (valid? result))
    (is (= 10.00M (:price retrieved))
        "The retrieved map has the correct values")))

(deftest delete-a-price
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

(def ^:private account-meta-context
  (-> basic-context
      (update-in [:commodities] concat [{:name "Apple, Inc."
                                         :type :stock
                                         :symbol "AAPL"
                                         :exchange :nasdaq
                                         :entity-id "Personal"}])
      (update-in [:accounts] concat [{:name "IRA"
                                      :entity-id "Personal"
                                      :type :asset}])
      (assoc :trades [{:type :buy
                       :commodity-id "AAPL"
                       :account-id "IRA"
                       :trade-date (t/local-date 2015 1 1)
                       :shares 100M
                       :value 1000M}])))

(deftest creating-a-price-updates-account-meta-data
  (with-context account-meta-context
    (let [commodity (find-commodity "AAPL")
          price (prices/create {:commodity-id (:id commodity)
                                :trade-date (t/local-date 2015 1 2)
                                :price 12M})]

      (is (empty? (v/error-messages price))
          "The price is created successfully")
      (is (= 1200M (:value (accounts/find-by {:commodity-id (:id commodity)})))
          "The account value is correct after creating the price"))))

(def ^:private account-meta-context-for-update
  (-> account-meta-context
      (assoc :prices [{:trade-date (t/local-date 2015 2 1)
                       :commodity-id "AAPL"
                       :price 12M}])))

(deftest updating-a-price-updates-account-meta-data
  (with-context account-meta-context-for-update
    (is (= 1200M (:value (accounts/find-by {:name "AAPL"})))
        "The account value reflects the price before update")
    (let [price (find-price "AAPL" (t/local-date 2015 2 1))]
      (prices/update (assoc price :price 13M :trade-date (t/local-date 2016 1 1)))
      (is (= 1300M (:value (accounts/find-by {:name "AAPL"})))
        "The account value reflects the previous price after update")
      (is (seq-of-maps-like? [{:trade-date (t/local-date 2015 1 1)
                               :price 10M}
                              {:trade-date (t/local-date 2016 1 1)
                               :price 13M}]
                             (prices/search {:trade-date [:between (t/local-date 2015 1 1) (t/local-date 2016 12 31)]}
                                            {:sort [:trade-date]}))
          "The price is moved to the new partition without duplication"))))

(deftest deleting-a-price-updates-account-meta-data
  (with-context account-meta-context-for-update
    (is (= 1200M (:value (accounts/find-by {:name "AAPL"})))
        "The account value reflects the price before delete")
    (let [price (find-price "AAPL" (t/local-date 2015 2 1))]
      (prices/delete price)
      (is (= 1000M (:value (accounts/find-by {:name "AAPL"})))
        "The account value reflects the previous price after delete"))))
