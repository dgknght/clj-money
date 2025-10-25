(ns clj-money.otel
  (:require [clojure.string :as string]
            [camel-snake-kebab.core :refer [->snake_case_string]])
  (:import io.opentelemetry.api.GlobalOpenTelemetry
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
           (.end span#))))))
