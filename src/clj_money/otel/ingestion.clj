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

; A statement read by a local model can take minutes, so the buckets
; recommended for gen_ai.client.operation.duration are extended past the
; five minute timeout
(def ^:private model-duration-buckets
  [0.01 0.02 0.04 0.08 0.16 0.32 0.64 1.28 2.56 5.12 10.24 20.48 40.96
   81.92 163.84 327.68])

(defn- model-duration
  [phase description]
  (otel/histogram (format "clj_money.ingestion.model.%s.duration" phase)
                  :description description
                  :unit "s"
                  :buckets model-duration-buckets))

; Timings reported by the model server (e.g., ollama), separating the time
; spent loading the model from the time spent reading the prompt and
; generating the response. The tokens per second generated is the output
; tokens recorded in gen_ai.client.token.usage over the eval duration.
(def ^:private model-durations
  {:total (model-duration "total" "Time the model server spent on the request")
   :load (model-duration "load" "Time the model server spent loading the model")
   :prompt-eval (model-duration "prompt_eval" "Time the model server spent reading the prompt")
   :eval (model-duration "eval" "Time the model server spent generating the response")})

(defn- record-usage
  [span attributes {:keys [model input-type input-tokens output-tokens durations]}]
  (let [attributes (assoc attributes
                          "gen_ai.response.model" model
                          "clj_money.ingestion.input_type" input-type)]
    (when model
      (otel/set-attribute span "gen_ai" "response" "model" model))
    (when input-type
      (otel/set-attribute span :clj-money :ingestion :input-type input-type))
    (doseq [[token-type tokens] {"input" input-tokens
                                 "output" output-tokens}
            :when tokens]
      (otel/set-attribute span "gen_ai" "usage" (str token-type "_tokens") tokens)
      (otel/record token-usage
                   tokens
                   (assoc attributes "gen_ai.token.type" token-type)))
    (doseq [[phase seconds] durations
            :let [instrument (model-durations phase)]
            :when (and instrument seconds)]
      (otel/set-attribute span
                          :clj-money :ingestion :model (str (name phase) "-duration")
                          (double seconds))
      (otel/record instrument seconds attributes))))

(defn- elapsed-seconds
  [started]
  (/ (- (System/nanoTime) started) 1e9))

(defn- observe
  [document read-fn {::ing/keys [provider] :keys [model]} opts]
  (with-tracing [span (str "ingestion/read-" document)]
    (let [attributes {"gen_ai.operation.name" "generate_content"
                      "clj_money.ingestion.document" document
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

(defn- observable-receipt-reader
  "Decorates the receipt reader so that each call to the model is traced and
  its duration and token usage are exported as metrics. A reader reports
  token usage by attaching :clj-money.ingestion/usage metadata to the
  result, with the keys :model, :input-tokens, and :output-tokens, and,
  optionally, :input-type (\"text\" or \"image\") and :durations, the
  timings reported by the model server in seconds (:total, :load,
  :prompt-eval and :eval)."
  [reader config]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (observe "receipt"
               #(ing/read-receipt reader source entity opts)
               config
               opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defn- observable-statement-reader
  "Decorates the statement reader as observable-receipt-reader decorates a
  receipt reader"
  [reader config]
  (reify
    ing/StatementReader
    (read-statement [_ source entity opts]
      (observe "statement"
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
       (take-while map?)
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
