(ns clj-money.prices.alpha-vantage-test
  (:require [clojure.test :refer [deftest is] :as test]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-http.core :as http]
            [dgknght.app-lib.test]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.prices.alpha-vantage :as alpha-vantage]))

(def mocks
  {#"alpha-vantage\.p\.rapidapi\.com"
   (fn [_]
     {:status 200
      :body (io/input-stream "resources/fixtures/alpha_vantage/digital_currency_daily_response.json")})})

(deftest get-price-quote
  (with-web-mocks [calls] mocks
    (is (comparable? {:price/value 43681.73M
                      :price/trade-date (t/local-date 2022 1 6)
                      :commodity/symbol "BTC"
                      :commodity/exchange :currency}
                     (alpha-vantage/get-quote "BTC")))
    (is (called-with-headers?
          :once
          calls
          {"X-Rapidapi-Host" "alpha-vantage.p.rapidapi.com"
           "X-Rapidapi-Key" "alpha-vantage-api-key"})
        "It includes the headers required by the API")))
