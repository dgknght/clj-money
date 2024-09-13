(ns clj-money.prices.yahoo-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.prices.yahoo :as yahoo]))

(def mocks
  {#"yh-finance\.p\.rapidapi\.com\/market\/v2\/get-quotes"
   (fn [_]
     {:status 200
      :body (io/input-stream "resources/fixtures/yahoo/get-quotes-response.json")})})

(deftest get-price-quotes
  (with-web-mocks [calls] mocks
    (is (seq-of-maps-like? [{:symbol "AMD"
                             :regularMarketTime (t/local-date 2021 12 10)
                             :regularMarketPrice 137.51M}
                            {:symbol "IBM"
                             :regularMarketTime (t/local-date 2021 12 10)
                             :regularMarketPrice 123.55M}
                            {:symbol "AAPL"
                             :regularMarketTime (t/local-date 2021 12 10)
                             :regularMarketPrice 177.78M}]
                           (yahoo/get-quotes ["AMD" "IBM" "AAPL"])))
    (is (called-with-headers? {"X-Rapidapi-Host" "yh-finance.p.rapidapi.com"
                               "X-Rapidapi-Key" "yahoo-api-key"}
                              :once
                              calls)
        "It includes the headers required by the API")))
