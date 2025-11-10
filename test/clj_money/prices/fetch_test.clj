(ns clj-money.prices.fetch-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.test-context :refer [with-context]]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.util :as util]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.entities.ref]
            [clj-money.entities :as e]
            [clj-money.db.ref]
            [clj-money.prices.fetch :as f]
            [clj-money.prices.alpha-vantage :as alpha]
            [clj-money.prices.yahoo :as yahoo]))

(use-fixtures :each reset-db)

(def ^:private alpha-data
  (index-by :commodity/symbol [{:price/value 10M
                                :price/trade-date (t/local-date 2016 1 1)
                                :commodity/symbol "AAPL"
                                :commodity/exchange :nasdaq}]))

(def ^:private yahoo-data
  (index-by :commodity/symbol [{:price/value 11M
                                :price/trade-date (t/local-date 2016 1 1)
                                :commodity/symbol "MSFT"
                                :commodity/exchange :nasdaq}]))

(def ^:private expected-prices
  #{{:price/commodity {:id 1
                       :commodity/symbol "AAPL"
                       :commodity/exchange :nasdaq
                       :commodity/type :stock}
     :price/value 10M
     :price/trade-date (t/local-date 2016 1 1)}
    {:price/commodity {:id 2
                       :commodity/symbol "MSFT"
                       :commodity/exchange :nasdaq
                       :commodity/type :stock}
     :price/value 11M
     :price/trade-date (t/local-date 2016 1 1)}})

(def ^:private commodities
  [{:id 1
    :commodity/symbol "AAPL"
    :commodity/exchange :nasdaq
    :commodity/type :stock}
   {:id 2
    :commodity/symbol "MSFT"
    :commodity/exchange :nasdaq
    :commodity/type :stock}])

(deftest fetch-prices-from-alpha-vantage
  (let [calls (atom {:alpha []
                     :yahoo []})]
    (with-redefs [alpha/get-quote (fn [symbol]
                                    (swap! calls update-in [:alpha] conj symbol)
                                    (alpha-data symbol))
                  yahoo/get-quotes (fn [symbols]
                                     (swap! calls update-in [:yahoo] conj symbols)
                                     (->> symbols
                                          (map yahoo-data)
                                          (filter identity)))]
      (is (= expected-prices
             (set (with-fixed-time "2016-01-01T12:00:00Z"
                      (f/fetch commodities))))
          "The prices are returned")
      (is (seq-of-maps-like? [{:cached-price/value 10M
                               :cached-price/symbol "AAPL"}
                              {:cached-price/value 11M
                               :cached-price/symbol "MSFT"}]
                             (e/select (util/entity-type {} :cached-price)
                                       {:sort [:cached-price/symbol]}))
          "The cached price records are created")
      (is (= #{"MSFT" "AAPL"}
             (set (:alpha @calls)))
          "The Alpha Vantage API is called once for each symbol")
      (is (= #{["MSFT"]}
             (set (:yahoo @calls)))
          "The Yahoo API is called once with all symbols for which Alpha Vantage does not return a response"))))

(def ^:private cache-ctx
  [#:cached-price{:trade-date (t/local-date 2016 1 1)
                  :symbol "MSFT"
                  :exchange :nasdaq
                  :value 11M}
   #:cached-price{:trade-date (t/local-date 2016 1 1)
                  :symbol "AAPL"
                  :exchange :nasdaq
                  :value 10M}])

(deftest get-quotes-cached-locally
  (with-context cache-ctx
    (let [calls (atom {:alpha []
                       :yahoo []})]
      (with-redefs [alpha/get-quote (fn [symbol]
                                      (swap! calls update-in [:alpha] conj symbol)
                                      nil)
                    yahoo/get-quotes (fn [symbols]
                                       (swap! calls update-in [:yahoo] conj symbols)
                                       [])]
        (is (= expected-prices
               (set (with-fixed-time "2016-01-01T12:00:00Z"
                      (f/fetch commodities))))
            "The prices are returned")
        (is (zero? (count (:alpha @calls)))
            "The Alpha Vantage API is not called")
        (is (zero? (count (:yahoo @calls)))
            "The Yahoo API is not called")))))
