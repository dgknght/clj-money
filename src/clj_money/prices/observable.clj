(ns clj-money.prices.observable
  "A price provider that decorates another provider so that each fetch is
  logged and traced, and its duration and outcome are exported as metrics.

    {:clj-money.prices/provider :clj-money.prices.observable/provider
     :name \"alpha-vantage\"
     :inner {:clj-money.prices/provider :clj-money.prices.alpha-vantage/provider
             :api-key \"...\"}}

  The log entries, span and metrics identify the provider by :name, or by
  the :clj-money.prices/provider key of :inner when :name is omitted."
  (:require [clojure.tools.logging :as log]
            [clj-money.dates :as dates]
            [clj-money.otel :as otel :refer [with-tracing]]
            [clj-money.prices :as prices]))

(def ^:private fetch-duration
  (otel/histogram "clj_money.prices.fetch.duration"
                  :description "Duration of calls to fetch a price from a price provider"
                  :unit "s"
                  :buckets otel/duration-buckets))

(defn- elapsed-seconds
  [started]
  (/ (- (System/nanoTime) started) 1e9))

(defn- trace-request
  [span {sym :commodity/symbol :commodity/keys [exchange]} {:keys [date]} provider-name]
  (when sym
    (otel/set-attribute span "commodity" "symbol" sym))
  (when exchange
    (otel/set-attribute span "commodity" "exchange" (name exchange)))
  (when date
    (otel/set-attribute span "price" "requested-date" (dates/serialize-local-date date)))
  (when provider-name
    (otel/set-attribute span "price" "provider" provider-name)))

(defn- trace-result
  [span {:price/keys [trade-date]} outcome]
  (otel/set-attribute span "price" "outcome" outcome)
  (when trade-date
    (otel/set-attribute span "price" "trade-date" (dates/serialize-local-date trade-date))))

(defn- log-prefix
  [provider-name]
  (if provider-name
    (format "[%s] " provider-name)
    ""))

(defn- fetch*
  [inner provider-name {:as commodity sym :commodity/symbol :commodity/keys [exchange]} opts]
  (with-tracing [span "prices/fetch"]
    (trace-request span commodity opts provider-name)
    (log/infof "%sfetching price for commodity %s: %s"
               (log-prefix provider-name)
               sym
               (pr-str commodity))
    (let [attributes {"clj_money.price.provider" provider-name
                      "clj_money.commodity.exchange" exchange}
          started (System/nanoTime)]
      (try
        (let [result (prices/fetch inner commodity opts)
              outcome (if result "found" "not_found")]
          (otel/record fetch-duration
                       (elapsed-seconds started)
                       (assoc attributes "clj_money.price.outcome" outcome))
          (trace-result span result outcome)
          (log/infof "%sfetched price %s for commodity %s"
                     (log-prefix provider-name)
                     (pr-str result)
                     sym)
          result)
        (catch Exception e
          (otel/record fetch-duration
                       (elapsed-seconds started)
                       (assoc attributes
                              "clj_money.price.outcome" "error"
                              "error.type" (.getName (class e))))
          (log/errorf e
                      "%sunable to fetch the price for commodity %s"
                      (log-prefix provider-name)
                      sym)
          (throw e))))))

(defmethod prices/provider ::provider
  [{:keys [inner] provider-name :name}]
  {:pre [inner]}
  (let [p (if (satisfies? prices/Provider inner)
            inner
            (prices/provider inner))
        provider-name (or provider-name
                          (some-> inner ::prices/provider symbol str))]
    (reify
      prices/Provider
      (fetch [_ commodity opts]
        (fetch* p provider-name commodity opts)))))
