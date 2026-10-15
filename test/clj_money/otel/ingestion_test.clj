(ns clj-money.otel.ingestion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-money.otel :as otel]
            [clj-money.ingestion :as ing]
            [clj-money.otel.ingestion :as otel-ing]))

(defn- stub-reader
  [f]
  (reify ing/Reader
    (read-receipt [_ source entity opts] (f source entity opts))
    (close [_])))

(defn- read-with-metrics
  [f & {:as opts}]
  (let [recorded (atom [])
        reader (otel-ing/observable-reader (stub-reader f)
                                      {::ing/provider :clj-money.ingestion.ollama/ollama
                                       :model "configured-model"})]
    (with-redefs [otel/record (fn [_instrument value attributes]
                                (swap! recorded conj [value attributes]))]
      [(try
         (ing/read-receipt reader "source" {:id 1} opts)
         (catch Exception e e))
       @recorded])))

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
            "gen_ai.provider.name" "ollama"
            "gen_ai.request.model" "configured-model"}
           duration-attrs)
        "The duration is recorded with the provider and model")
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
