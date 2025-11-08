(ns clj-money.prices.fetch-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.core :refer [index-by]]
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
  (index-by :commodity-symbol [{:price/value 11M
                                :price/trade-date (t/local-date 2016 1 1)
                                :commodity/symbol "MSFT"
                                :commodity/exchange :nasdaq}]))

(deftest fetch-prices-from-alpha-vantage
  (with-redefs [alpha/get-quote (fn [symbol]
                                  (alpha-data symbol))
                yahoo/get-quotes (fn [symbols]
                                   (->> symbols
                                        (map yahoo-data)
                                        (filter identity)))]
    (is (= [{:price/commodity {:id 1
                               :commodity/symbol "AAPL"
                               :commodity/exchange :nasdaq
                               :commodity/type :stock}
             :price/value 10M
             :price/trade-date (t/local-date 2016 1 1)}]
           (f/fetch [{:id 1
                      :commodity/symbol "AAPL"
                      :commodity/exchange :nasdaq
                      :commodity/type :stock}]))
        "The prices are returned")
    (is (comparable? {:cached-price/value 10M}
                     (e/find-by {:cached-price/symbol "AAPL"}))
        "The cached price record is created")))
