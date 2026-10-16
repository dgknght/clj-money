(ns clj-money.otel
  (:require [clojure.string :as string]
            [camel-snake-kebab.core :refer [->snake_case_string]])
  (:import io.opentelemetry.api.GlobalOpenTelemetry
           [io.opentelemetry.api.common Attributes AttributesBuilder]
           [io.opentelemetry.api.metrics
            Meter
            DoubleHistogram
            ObservableLongGauge
            ObservableLongMeasurement]
           java.util.function.Consumer
           [io.opentelemetry.api.trace Tracer Span StatusCode]))

(def scope-name "clj-money")
(def scope-version "1.0.0")

(defn- ->attribute-key
  [ks]
  (->> ks
       (map ->snake_case_string)
       (string/join ".")))

(defn set-attribute
  [^Span span & keys-and-val]
  (let [k (->attribute-key (butlast keys-and-val))
        v (last keys-and-val)]
    (.setAttribute span k v)))

(defn record-exception
  [^Span span ^Throwable e]
  (.recordException span e))

(def status-map
  {:ok StatusCode/OK
   :error StatusCode/ERROR})

(defn set-status
  [^Span span status description]
  (.setStatus span (status-map status) description))

(defmacro with-tracing
  [bindings & body]
  (let [span-name (second bindings)]
    `(let [tracer# ^Tracer (GlobalOpenTelemetry/getTracer ~scope-name ~scope-version)
           span# ^Span (.startSpan (.spanBuilder tracer# ~span-name))
           scope# (.makeCurrent span#)
           f# (fn* [~(first bindings)]
                   ~@body)]
       (try
         (f# span#)
         (catch Exception e#
           (record-exception span# e#)
           (set-status span#
                       :error
                       (or (ex-message e#)
                           "Unknown error"))
           (throw e#))
         (finally
           (.close scope#)
           (.end span#))))))

(defn- meter
  ^Meter []
  (GlobalOpenTelemetry/getMeter scope-name))

(defn- ->attributes
  "Converts a map into OpenTelemetry attributes, omitting nil values. Keys
  are used as-is, so that semantic convention names like
  \"gen_ai.request.model\" pass through unchanged."
  ^Attributes [m]
  (let [^AttributesBuilder builder (Attributes/builder)]
    (doseq [[k v] m :when (some? v)]
      (let [^String k (name k)
            ^String v (if (keyword? v) (name v) (str v))]
        (.put builder k v)))
    (.build builder)))

(def duration-buckets
  "Bucket boundaries, in seconds, for duration histograms, as recommended
  by the OpenTelemetry semantic conventions. Without them, the SDK's
  defaults (sized for milliseconds) put nearly every duration in seconds
  into the first bucket."
  [0.005 0.01 0.025 0.05 0.075 0.1 0.25 0.5 0.75 1 2.5 5 7.5 10])

(defn histogram
  "Returns a delayed double histogram with the specified name, created
  lazily so that the global OpenTelemetry instance is not resolved at load
  time. :buckets is a sequence of bucket boundaries (see duration-buckets)."
  [instrument-name & {:keys [description unit buckets]}]
  (delay
    (cond-> (.histogramBuilder (meter) instrument-name)
      description (.setDescription description)
      unit (.setUnit unit)
      buckets (.setExplicitBucketBoundariesAdvice (mapv double buckets))
      true .build)))

(defn record
  "Records the value in a histogram created with histogram"
  [instrument value attributes]
  (.record ^DoubleHistogram @instrument (double value) (->attributes attributes)))

(defn gauge
  "Registers an observable gauge with the specified name. When metrics are
  collected, f is called with no arguments and returns a sequence of
  [value attributes] pairs, one for each value to report. Returns the gauge,
  which must be closed (see close-gauge) to stop observing."
  ^ObservableLongGauge [instrument-name f & {:keys [description unit]}]
  (-> (cond-> (.ofLongs (.gaugeBuilder (meter) instrument-name))
        description (.setDescription description)
        unit (.setUnit unit))
      (.buildWithCallback
        (reify Consumer
          (accept [_ measurement]
            (doseq [[value attributes] (f)]
              (.record ^ObservableLongMeasurement measurement
                       (long value)
                       (->attributes attributes))))))))

(defn close-gauge
  "Stops observing a gauge created with gauge"
  [^ObservableLongGauge gauge]
  (.close gauge))
