(ns clj-money.prices.alpha-vantage-test
  (:require [clojure.test :refer [deftest is] :as test]
            [clojure.java.io :as io]
            [clj-time.core :as t]
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
    (is (comparable? {:meta-data {:information "Daily Prices and Volumes for Digital Currency"
                                  :digital-currency-code "BTC"
                                  :digital-currency-name "Bitcoin"
                                  :market-code "USD"
                                  :market-name "United States Dollar"
                                  :last-refreshed (t/date-time 2022 1 6)
                                  :time-zone "UTC"}
                      :time-series {(t/local-date 2022 1 6) {"USD" {:open 43451.14M
                                                                    :high 43689.99M
                                                                    :low 43261.62M
                                                                    :close 43681.73M
                                                                    :market-cap 1505.96873M}
                                                             :volume 1505.96873M}
                                    (t/local-date 2022 1 5) {"USD" {:open 45832.01M
                                                                    :high 47070M
                                                                    :low 42500M
                                                                    :close 43451.13M
                                                                    :market-cap 51784.11857M}
                                                             :volume 51784.11857M}}}
                     (alpha-vantage/get-quote "BTC")))
    (is (called-with-headers?
          :once
          calls
          {"X-Rapidapi-Host" "alpha-vantage.p.rapidapi.com"
           "X-Rapidapi-Key" "alpha-vantage-api-key"})
        "It includes the headers required by the API")))
