(ns clj-money.prices.observable-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.tools.logging.test :as log-test]
            [java-time.api :as t]
            [clj-money.otel :as otel]
            [clj-money.prices :as prices]
            [clj-money.prices.observable :as obs]))

(defn- stub-provider
  [{:keys [fetch]}]
  (reify prices/Provider
    (fetch [_ commodity opts] (fetch commodity opts))))

(defmethod prices/provider ::stub
  [config]
  (stub-provider config))

(def ^:private commodity
  #:commodity{:symbol "AAPL"
              :exchange :nasdaq})

(def ^:private price
  #:price{:trade-date (t/local-date 2015 3 2)
          :value 10.01M})

(defn- fetch-with-telemetry
  "Fetches a price for commodity from an observable provider with the given
  config (:inner and, optionally, :name), and returns the result (or the
  exception thrown), the recorded metrics, as [value attributes] pairs, the
  span attributes and the logged messages."
  [config & {:as opts}]
  (let [recorded (atom [])
        span-attributes (atom {})]
    (with-redefs [otel/record (fn [_instrument value attributes]
                                (swap! recorded conj [value attributes]))
                  otel/set-attribute (fn [_span & keys-and-val]
                                       (swap! span-attributes
                                              assoc
                                              (vec (butlast keys-and-val))
                                              (last keys-and-val)))]
      (log-test/with-log
        [(try
           (prices/fetch (prices/provider (assoc config
                                                 ::prices/provider
                                                 ::obs/provider))
                         commodity
                         (or opts {}))
           (catch Exception e e))
         @recorded
         @span-attributes
         (mapv :message (log-test/the-log))]))))

(defn- stub
  [fetch]
  {::prices/provider ::stub
   :fetch fetch})

(deftest record-the-duration-and-outcome-of-a-fetch
  (let [calls (atom [])
        [result
         [[duration attrs] :as recorded]
         span-attrs
         messages] (fetch-with-telemetry
                     {:inner (stub (fn [& args]
                                     (swap! calls conj args)
                                     price))}
                     :date (t/local-date 2015 3 2))]
    (is (= price result)
        "The result of the decorated provider is returned")
    (is (= [[commodity {:date (t/local-date 2015 3 2)}]]
           @calls)
        "The commodity and options are passed to the decorated provider")
    (is (= 1 (count recorded))
        "The duration is recorded once")
    (is (and (number? duration)
             (not (neg? duration)))
        "The duration is recorded")
    (is (= {"clj_money.price.provider" "clj-money.prices.observable-test/stub"
            "clj_money.commodity.exchange" :nasdaq
            "clj_money.price.outcome" "found"}
           attrs)
        "The duration is recorded with the provider, exchange and outcome")
    (is (= {["commodity" "symbol"] "AAPL"
            ["commodity" "exchange"] "nasdaq"
            ["price" "provider"] "clj-money.prices.observable-test/stub"
            ["price" "requested-date"] "2015-03-02"
            ["price" "outcome"] "found"
            ["price" "trade-date"] "2015-03-02"}
           span-attrs)
        "The request and the result are added to the span")
    (is (= 2 (count messages))
        "The request and the result are logged")
    (is (every? #(.startsWith ^String % "[clj-money.prices.observable-test/stub] ")
                messages)
        "The log entries are prefixed with the provider")))

(deftest identify-the-provider-by-the-configured-name
  (let [[_ [[_ attrs]] span-attrs messages]
        (fetch-with-telemetry {:name "stub-api"
                               :inner (stub (constantly price))})]
    (is (= "stub-api" (attrs "clj_money.price.provider"))
        "The name is recorded in the metric attributes")
    (is (= "stub-api" (span-attrs ["price" "provider"]))
        "The name is added to the span")
    (is (seq messages)
        "The fetch is logged")
    (is (every? #(.startsWith ^String % "[stub-api] ") messages)
        "The log entries are prefixed with the name")))

(deftest record-a-fetch-that-finds-no-price
  (let [[result [[_ attrs]] span-attrs] (fetch-with-telemetry
                                          {:inner (stub (constantly nil))})]
    (is (nil? result)
        "Nil is returned")
    (is (= "not_found" (attrs "clj_money.price.outcome"))
        "The duration is recorded with the not-found outcome")
    (is (= "not_found" (span-attrs ["price" "outcome"]))
        "The outcome is added to the span")
    (is (not (contains? span-attrs ["price" "trade-date"]))
        "No trade date is added to the span")))

(deftest record-a-failed-fetch
  (let [[result recorded span-attrs messages]
        (fetch-with-telemetry
          {:name "stub-api"
           :inner (stub (fn [& _]
                          (throw (ex-info "Unable to fetch the quote" {}))))})]
    (is (= "Unable to fetch the quote" (ex-message result))
        "The exception is rethrown")
    (testing "the duration is recorded with the error"
      (is (= 1 (count recorded)))
      (is (= {"clj_money.price.outcome" "error"
              "error.type" "clojure.lang.ExceptionInfo"}
             (select-keys (second (first recorded))
                          ["clj_money.price.outcome"
                           "error.type"]))))
    (is (not (contains? span-attrs ["price" "outcome"]))
        "No outcome is added to the span (the span records the exception)")
    (is (= "[stub-api] unable to fetch the price for commodity AAPL"
           (last messages))
        "The failure is logged with the name")))

(deftest decorate-a-provider-instance
  (let [[result [[_ attrs]] span-attrs messages]
        (fetch-with-telemetry {:inner (stub-provider {:fetch (constantly price)})})]
    (is (= price result)
        "The result of the decorated provider is returned")
    (is (nil? (attrs "clj_money.price.provider"))
        "The provider is omitted from the metric attributes")
    (is (not (contains? span-attrs ["price" "provider"]))
        "The provider is omitted from the span")
    (is (.startsWith ^String (first messages) "fetching price for commodity AAPL")
        "The log entries have no provider prefix")))

(deftest an-inner-provider-is-required
  (is (thrown? AssertionError
               (prices/provider {::prices/provider ::obs/provider}))))

(deftest fetch-with-the-opentelemetry-api
  (is (= price
         (prices/fetch (prices/provider {::prices/provider ::obs/provider
                                         :inner (stub (constantly price))})
                       commodity
                       {:date (t/local-date 2015 3 2)}))
      "The price is fetched without error when the telemetry is recorded"))
