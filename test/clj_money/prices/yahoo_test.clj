(ns clj-money.prices.yahoo-test
  (:require [clojure.test :refer [deftest is] :as test]
            [clojure.java.io :as io]
            [java-time.api :as t]
            [clj-http.core :as http]
            [dgknght.app-lib.test]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.prices.yahoo :as yahoo]
            [clojure.core :as c]))

(def mocks
  {#"yh-finance\.p\.rapidapi\.com\/market\/v2\/get-quotes"
   (fn [_]
     {:status 200
      :body (io/input-stream "resources/fixtures/yahoo/get-quotes-response.json")})})

(deftest get-price-quotes
  (with-web-mocks [calls] mocks
    (let [result (yahoo/get-quotes ["AMD" "IBM" "AAPL"])]
      (is (seq-of-maps-like? [{:symbol "AMD"
                               :regularMarketTime (t/instant "2021-12-10T19:08:35Z")
                               :regularMarketPrice 137.51M}
                              {:symbol "IBM"
                               :regularMarketTime (t/instant "2021-12-10T19:08:35Z")
                               :regularMarketPrice 123.55M}
                              {:symbol "AAPL"
                               :regularMarketTime (t/instant "2021-12-10T19:08:41Z")
                               :regularMarketPrice 177.78M}]
                             result)))
    (is (called-with-headers? :once
                              calls
                              {"X-Rapidapi-Host" "yh-finance.p.rapidapi.com"
                               "X-Rapidapi-Key" "yahoo-api-key"})
        "It includes the headers required by the API")))
