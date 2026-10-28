(ns clj-money.prices.fallback-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.tools.logging.test :as log-test]
            [java-time.api :as t]
            [clj-http.core :as http]
            [dgknght.app-lib.web-mocks :refer [with-web-mocks]]
            [clj-money.prices.mock :refer [Inspectable
                                           inspect]]
            [clj-money.prices :as prices]
            [clj-money.prices.eodhd :as eod]
            [clj-money.prices.fallback :as fb]))

(defn- provider
  [result]
  (let [calls (atom [])]
    (reify
      prices/Provider
      (fetch [_ commodity opts]
        (swap! calls conj [commodity opts])
        (when result
          (assoc result
                 :price/trade-date (:date opts)
                 :price/commodity commodity)))

      Inspectable
      (inspect [_] @calls))))

(def ^:private commodity
  #:commodity{:symbol "AAPL"
              :exchange :nasdaq})

(deftest fallback-to-another-provider
  (let [p1 (provider nil)
        p2 (provider #:price{:value 10M})
        p (prices/provider {::prices/provider ::fb/provider
                            :providers [p1 p2]})]
    (is (= #:price{:trade-date (t/local-date 2020 1 1)
                   :value 10M
                   :commodity commodity}
           (prices/fetch p
                         commodity
                         {:date (t/local-date 2020 1 1)})))
    (is (= 1 (count (inspect p1)))
        "The 1st provider is invoked once")
    (is (= 1 (count (inspect p2)))
        "The 2nd provider is invoked once")))

(deftest succeed-on-the-first-provider
  (let [p1 (provider #:price{:value 10M})
        p2 (provider #:price{:value 20M})
        p (prices/provider {::prices/provider ::fb/provider
                            :providers [p1 p2]})]
    (is (= #:price{:trade-date (t/local-date 2020 1 1)
                   :value 10M
                   :commodity commodity}
           (prices/fetch p
                         commodity
                         {:date (t/local-date 2020 1 1)})))
    (is (= 1 (count (inspect p1)))
        "The 1st provider is invoked once")
    (is (= 0 (count (inspect p2)))
        "The 2nd provider is not invoked")))

(def ^:private not-found-mocks
  {#"^https:\/\/eodhd\.com"
   (fn [_]
     {:status 404
      :body (io/input-stream (.getBytes "Ticker Not Found."))})})

(deftest fallback-to-another-provider-after-an-error
  (let [p2 (provider #:price{:value 10M})
        p (prices/provider {::prices/provider ::fb/provider
                            :providers [{::prices/provider ::eod/provider
                                         :api-key "test-api-key"}
                                        p2]})]
    (with-web-mocks [calls] not-found-mocks
      (log-test/with-log
        (is (= #:price{:trade-date (t/local-date 2020 1 1)
                       :value 10M
                       :commodity commodity}
               (prices/fetch p
                             commodity
                             {:date (t/local-date 2020 1 1)}))
            "The price from the 2nd provider is returned")
        (is (= 1 (count @calls))
            "The 1st provider is invoked")
        (is (= 1 (count (inspect p2)))
            "The 2nd provider is invoked")
        (is (log-test/logged? 'clj-money.prices.fallback
                              :warn
                              "price provider 1 of 2 failed for commodity AAPL: clj-http: status 404")
            "The failure is logged as a warning")
        (is (every? (comp nil? :throwable) (log-test/the-log))
            "The stack trace is not logged")))))

(deftest return-nil-when-every-provider-fails
  (let [failing (reify prices/Provider
                  (fetch [_ _commodity _opts]
                    (throw (ex-info "Service unavailable" {}))))
        p (prices/provider {::prices/provider ::fb/provider
                            :providers [failing failing]})]
    (log-test/with-log
      (is (nil? (prices/fetch p
                              commodity
                              {:date (t/local-date 2020 1 1)}))
          "Nil is returned")
      (is (= ["price provider 1 of 2 failed for commodity AAPL: Service unavailable"
              "price provider 2 of 2 failed for commodity AAPL: Service unavailable"]
             (->> (log-test/the-log)
                  (filter #(= :warn (:level %)))
                  (mapv :message)))
          "Each failure is logged as a warning"))))
