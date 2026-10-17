(ns clj-money.otel.ingestion
  (:require [clj-money.otel :as otel :refer [with-tracing]]
            [clj-money.ingestion :as ing]))

; Instrument names and attributes follow the OpenTelemetry semantic
; conventions for generative AI clients
(def ^:private operation-duration
  (otel/histogram "gen_ai.client.operation.duration"
                  :description "Duration of calls to the model"
                  :unit "s"))

(def ^:private token-usage
  (otel/histogram "gen_ai.client.token.usage"
                  :description "Number of tokens used in calls to the model"
                  :unit "{token}"))

(defn- record-usage
  [span attributes {:keys [model input-tokens output-tokens]}]
  (let [attributes (assoc attributes "gen_ai.response.model" model)]
    (when model
      (otel/set-attribute span "gen_ai" "response" "model" model))
    (doseq [[token-type tokens] {"input" input-tokens
                                 "output" output-tokens}
            :when tokens]
      (otel/set-attribute span "gen_ai" "usage" (str token-type "_tokens") tokens)
      (otel/record token-usage
                   tokens
                   (assoc attributes "gen_ai.token.type" token-type)))))

(defn- elapsed-seconds
  [started]
  (/ (- (System/nanoTime) started) 1e9))

(defn- observe
  [span-name read-fn {::ing/keys [provider] :keys [model]} opts]
  (with-tracing [span span-name]
    (let [attributes {"gen_ai.operation.name" "generate_content"
                      "gen_ai.provider.name" (some-> provider name)
                      "gen_ai.request.model" (or (:model opts) model)}
          started (System/nanoTime)]
      (try
        (let [result (read-fn)]
          (otel/record operation-duration
                       (elapsed-seconds started)
                       attributes)
          (when-let [usage (::ing/usage (meta result))]
            (record-usage span attributes usage))
          result)
        (catch Exception e
          (otel/record operation-duration
                       (elapsed-seconds started)
                       (assoc attributes
                              "error.type"
                              (.getName (class e))))
          (throw e))))))

(defn observable-receipt-reader
  "Decorates the receipt reader so that each call to the model is traced and
  its duration and token usage are exported as metrics. A reader reports
  token usage by attaching :clj-money.ingestion/usage metadata to the
  result, with the keys :model, :input-tokens, and :output-tokens."
  [reader config]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (observe "ingestion/read-receipt"
               #(ing/read-receipt reader source entity opts)
               config
               opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defn observable-statement-reader
  "Decorates the statement reader as observable-receipt-reader decorates a
  receipt reader"
  [reader config]
  (reify
    ing/StatementReader
    (read-statement [_ source entity opts]
      (observe "ingestion/read-statement"
               #(ing/read-statement reader source entity opts)
               config
               opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defn- decorated-config
  "Returns the config of the reader that calls the model, the innermost of
  the readers composed in the config"
  [config]
  (->> config
       (iterate :reader)
       (take-while some?)
       last))

(defmethod ing/receipt-reader ::observable
  [{:keys [reader] :as config}]
  {:pre [reader]}
  (observable-receipt-reader (ing/receipt-reader reader)
                             (decorated-config config)))

(defmethod ing/statement-reader ::observable
  [{:keys [reader] :as config}]
  {:pre [reader]}
  (observable-statement-reader (ing/statement-reader reader)
                               (decorated-config config)))
