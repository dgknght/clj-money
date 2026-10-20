(ns clj-money.otel.ingestion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-money.otel :as otel]
            [clj-money.ingestion :as ing]
            [clj-money.otel.ingestion :as otel-ing]))

(defn- stub-reader
  [{:keys [read]}]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts] (read source entity opts))
    ing/StatementReader
    (read-statement [_ source entity opts] (read source entity opts))
    ing/Closeable
    (close [_])))

(defmethod ing/receipt-reader ::stub
  [config]
  (stub-reader config))

(defmethod ing/statement-reader ::stub
  [config]
  (stub-reader config))

(defn- observable
  [read]
  {::ing/provider ::otel-ing/observable
   :reader {::ing/provider ::stub
            :model "configured-model"
            :read read}})

(defn- record-metrics
  [f]
  (let [recorded (atom [])]
    (with-redefs [otel/record (fn [_instrument value attributes]
                                (swap! recorded conj [value attributes]))]
      [(try
         (f)
         (catch Exception e e))
       @recorded])))

(defn- read-with-metrics
  [read & {:as opts}]
  (record-metrics #(ing/read-receipt (ing/receipt-reader (observable read))
                                     "source"
                                     {:id 1}
                                     opts)))

(deftest record-the-duration-and-token-usage-of-a-model-call
  (let [[result [[duration duration-attrs]
                 [input input-attrs]
                 [output output-attrs]
                 :as recorded]]
        (read-with-metrics
          (fn [& _]
            (with-meta {:total 10M}
                       {::ing/usage {:model "response-model"
                                     :input-tokens 1200
                                     :output-tokens 300}})))]
    (is (= {:total 10M} result)
        "The result of the decorated reader is returned")
    (is (= 3 (count recorded))
        "The duration and the input and output tokens are recorded")
    (is (and (number? duration)
             (not (neg? duration)))
        "The duration is recorded")
    (is (= {"gen_ai.operation.name" "generate_content"
            "gen_ai.provider.name" "stub"
            "gen_ai.request.model" "configured-model"}
           duration-attrs)
        "The duration is recorded with the provider and model of the reader it decorates")
    (is (= [1200 "input" "response-model"]
           [input
            (input-attrs "gen_ai.token.type")
            (input-attrs "gen_ai.response.model")])
        "The input tokens are recorded")
    (is (= [300 "output"]
           [output (output-attrs "gen_ai.token.type")])
        "The output tokens are recorded")))

(deftest the-model-specified-in-the-options-takes-precedence
  (let [[_ [[_ attrs]]] (read-with-metrics (constantly {})
                                           :model "requested-model")]
    (is (= "requested-model" (attrs "gen_ai.request.model")))))

(deftest record-the-duration-of-a-failed-model-call
  (let [[result [[_ attrs] :as recorded]]
        (read-with-metrics (fn [& _]
                             (throw (ex-info "Model unavailable" {}))))]
    (is (= "Model unavailable" (ex-message result))
        "The exception is rethrown")
    (testing "the duration is recorded with the error type"
      (is (= 1 (count recorded)))
      (is (= "clojure.lang.ExceptionInfo" (attrs "error.type"))))))

(deftest record-the-token-usage-of-reading-a-statement
  (let [[result recorded]
        (record-metrics
          #(ing/read-statement
             (ing/statement-reader
               (observable (fn [& _]
                             (with-meta {:closing-balance 10M}
                                        {::ing/usage {:model "response-model"
                                                      :input-tokens 5000
                                                      :output-tokens 900}}))))
             "source"
             {:id 1}
             {}))]
    (is (= {:closing-balance 10M} result)
        "The result of the decorated reader is returned")
    (is (= [5000 900]
           (->> recorded
                (filter (comp #(get % "gen_ai.token.type") second))
                (map first)))
        "The input and output tokens are recorded")))
