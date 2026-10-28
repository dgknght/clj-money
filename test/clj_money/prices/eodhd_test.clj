(ns clj-money.prices.eodhd-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [java-time.api :as t]
            [clj-http.core :as http]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [dgknght.app-lib.test-assertions]
            [clj-money.prices :as prices]
            [clj-money.prices.eodhd :as eod]))

(def mocks
  {#"^https:\/\/eodhd\.com"
   (fn [_]
     {:status 200
      :body (io/input-stream "fixtures/eodhd/response.json")})})

(deftest get-a-price
  (with-web-mocks [calls] mocks
    (is (comparable? {:price/value 294.39M
                      :price/trade-date (t/local-date 2024 1 3)}
                     (prices/fetch (prices/provider {::prices/provider ::eod/provider
                                                     :api-key "test-api-key"})
                                   {:commodity/symbol "MCD"
                                    :commodity/exchange :nyse}
                                   {:date (t/local-date 2024 1 3)}))
        "The price entity is returned")
    (is (called?
          :once
          calls
          #"^https://eodhd\.com\/api\/eod\/MCD.US")
        "The EODHD endpoint is called.")
    (is (called?
          :once
          calls
          #(= "test-api-key"
              (get-in % [:query-params :api_token])))
        "The API key is passed on the query string")))
