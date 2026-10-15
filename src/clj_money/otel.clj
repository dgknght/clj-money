(ns clj-money.otel
  (:require [clojure.string :as string]
            [camel-snake-kebab.core :refer [->snake_case_string]])
  (:import io.opentelemetry.api.GlobalOpenTelemetry
           [io.opentelemetry.api.common Attributes AttributesBuilder]
           [io.opentelemetry.api.metrics Meter DoubleHistogram]
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

(defn histogram
  "Returns a delayed double histogram with the specified name, created
  lazily so that the global OpenTelemetry instance is not resolved at load
  time."
  [instrument-name & {:keys [description unit]}]
  (delay
    (cond-> (.histogramBuilder (meter) instrument-name)
      description (.setDescription description)
      unit (.setUnit unit)
      true .build)))

(defn record
  "Records the value in a histogram created with histogram"
  [instrument value attributes]
  (.record ^DoubleHistogram @instrument (double value) (->attributes attributes)))
