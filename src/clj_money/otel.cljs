(ns clj-money.otel
  (:require ["opentelemetry" :as otel]))

(defonce ^:private initialized? (atom false))

(defn init!
  "Starts exporting browser traces (page loads, fetch/XHR requests and user
  interactions) to the given OTLP/HTTP traces endpoint. Safe to call again
  after a figwheel reload."
  [endpoint]
  (when (compare-and-set! initialized? false true)
    (otel/init #js {:url endpoint
                    :serviceName "clj-money-client"})))
