(ns clj-money.otel
  (:require [clojure.string :as string]
            [camel-snake-kebab.core :refer [->snake_case_string]])
  (:import io.opentelemetry.api.GlobalOpenTelemetry
           [io.opentelemetry.api.trace Tracer Span]))

(def scope-name "clj-money")
(def scope-version "1.0.0")

(defmacro with-tracing
  [bindings & body]
  (let [span-name (second bindings)]
    `(let [tracer# ^Tracer (GlobalOpenTelemetry/getTracer ~scope-name ~scope-version)
           span# ^Span (.startSpan (.spanBuilder tracer# ~span-name))
           f# (fn* [~(first bindings)]
                   ~@body)]
       (try
         (f# span#)
         (finally
           (.end span#))))))

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
