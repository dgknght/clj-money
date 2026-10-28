(ns clj-money.prices.alpha-vantage-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-http.core :as http]
            [dgknght.app-lib.test]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.prices :as prices]
            [clj-money.prices.alpha-vantage :as av]))

(def mocks
  {#"^https:\/\/www\.alphavantage\.co\/query"
   (fn [_]
     {:status 200
      :body (io/input-stream "fixtures/alpha_vantage/daily_time_series_response.json")})})

(deftest get-price
  (with-web-mocks [calls] mocks
    (is (comparable? {:price/value 251.46M
                      :price/trade-date (t/local-date 2026 10 21)}
                     (prices/fetch (prices/provider {::prices/provider ::av/provider
                                                     :api-key "test-alpha-key"})
                                   {:commodity/symbol "IBM"}
                                   {:date (t/local-date 2026 10 21)}))
        "The price entity is returned")
    (is (called?
          :once
          calls
          #"^https:\/\/www\.alphavantage\.co\/query\?")
        "The Alpha Vantage API is invoked")
    (is (called?
          :once
          calls
          #(= "TIME_SERIES_DAILY"
              (get-in % [:query-params :function])))
        "The TIME_SERIES_DAILY function is specified")
    (is (called?
            :once
            calls
            #(= "IBM"
                (get-in % [:query-params :symbol])))
          "The symbol is given")
    (is (called?
            :once
            calls
            #(= "test-alpha-key"
                (get-in % [:query-params :apikey])))
          "The api-key is given")))
