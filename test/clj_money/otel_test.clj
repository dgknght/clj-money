(ns clj-money.otel-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-money.otel-helpers :refer [capture collect-metrics]]
            [clj-money.otel :as otel :refer [with-tracing]])
  (:import io.opentelemetry.api.trace.Span))

(deftest trace-a-block-of-code
  (let [{:keys [result spans]}
        (capture #(with-tracing [span "test/operation"]
                    (otel/set-attribute span "test" "string-value" "abc")
                    (otel/set-attribute span :test :long-value 42)
                    (otel/set-attribute span "test" "boolean-value" true)
                    :done))
        [span :as spans] spans]
    (is (= :done result)
        "The value of the body is returned")
    (is (= 1 (count spans))
        "One span is created")
    (is (= "test/operation" (:name span))
        "The span is created with the specified name")
    (is (= {"test.string_value" "abc"
            "test.long_value" 42
            "test.boolean_value" true}
           (:attributes span))
        "The attributes are added with snake-case keys joined by dots")
    (is (= :unset (:status span))
        "The status is left unset")))

(deftest nest-traced-blocks-of-code
  (let [{[inner outer] :spans}
        (capture #(with-tracing [outer "test/outer"]
                    (is (= outer (Span/current))
                        "The span is the current span within the block")
                    (with-tracing [inner "test/inner"]
                      (is (= inner (Span/current))
                          "The nested span is the current span within the nested block"))
                    (is (= outer (Span/current))
                        "The outer span is restored when the nested block ends")))]
    (is (= ["test/inner" "test/outer"]
           (map :name [inner outer]))
        "A span is created for each block")
    (is (= (:span-id outer) (:parent-span-id inner))
        "The nested span is a child of the outer span")
    (is (nil? (:parent-span-id outer))
        "The outer span is a root span"))
  (is (not (.isValid (.getSpanContext (Span/current))))
      "There is no current span after the block ends"))

(deftest trace-a-block-of-code-that-throws-an-exception
  (let [{:keys [exception] [span] :spans}
        (capture #(with-tracing [_span "test/failure"]
                    (throw (ex-info "Something went wrong" {}))))]
    (is (= "Something went wrong" (ex-message exception))
        "The exception is rethrown")
    (is (= :error (:status span))
        "The span status is set to error")
    (is (= "Something went wrong" (:status-description span))
        "The span status describes the error")
    (is (= [{"exception.type" "clojure.lang.ExceptionInfo"
             "exception.message" "Something went wrong"}]
           (->> (:events span)
                (filter #(= "exception" (:name %)))
                (map #(select-keys (:attributes %)
                                   ["exception.type"
                                    "exception.message"]))))
        "The exception is recorded on the span")))

(deftest trace-a-block-of-code-that-throws-an-exception-without-a-message
  (let [{[span] :spans} (capture #(with-tracing [_span "test/failure"]
                                    (throw (RuntimeException.))))]
    (is (= :error (:status span)))
    (is (= "Unknown error" (:status-description span)))))

(deftest set-the-status-of-a-span
  (let [{[span] :spans} (capture #(with-tracing [span "test/status"]
                                    (otel/set-status span :ok "All good")))]
    (is (= :ok (:status span)))))

(deftest record-a-value-in-a-histogram
  (let [histogram (otel/histogram "clj_money.test.histogram"
                                  :description "A histogram used in tests"
                                  :unit "s"
                                  :buckets otel/duration-buckets)
        {{metric "clj_money.test.histogram"} :metrics}
        (capture #(do
                    (otel/record histogram 1.5 {"gen_ai.provider.name" "ollama"
                                                "gen_ai.token.type" :input
                                                "gen_ai.request.model" nil})
                    (otel/record histogram 0.5 {"gen_ai.provider.name" "ollama"
                                                "gen_ai.token.type" :input})))
        [point :as points] (:points metric)]
    (is (= {:description "A histogram used in tests"
            :unit "s"
            :type :histogram}
           (select-keys metric [:description :unit :type]))
        "The histogram is created with the specified description and unit")
    (is (= 1 (count points))
        "Values with the same attributes are aggregated")
    (is (= {"gen_ai.provider.name" "ollama"
            "gen_ai.token.type" "input"}
           (:attributes point))
        "Keyword values are recorded by name and nil values are omitted")
    (is (= [2 2.0]
           ((juxt :count :sum) point))
        "The values are recorded")
    (is (= (map double otel/duration-buckets)
           (:boundaries point))
        "The values are recorded in the specified buckets")))

(deftest observe-values-with-a-gauge
  (let [values (atom [[3 {"queue" "a"}]
                      [5 {"queue" "b"}]])
        gauge (otel/gauge "clj_money.test.gauge"
                          #(deref values)
                          :description "A gauge used in tests"
                          :unit "{message}")]
    (try
      (let [{:keys [points] :as metric} (get (collect-metrics) "clj_money.test.gauge")]
        (is (= {:description "A gauge used in tests"
                :unit "{message}"
                :type :long-gauge}
               (select-keys metric [:description :unit :type]))
            "The gauge is created with the specified description and unit")
        (is (= #{[3 {"queue" "a"}]
                 [5 {"queue" "b"}]}
               (set (map (juxt :value :attributes) points)))
            "A value is reported for each pair returned by the callback"))
      (testing "the callback is called on each collection"
        (reset! values [[4 {"queue" "a"}]])
        (is (= [4]
               (->> (get-in (collect-metrics) ["clj_money.test.gauge" :points])
                    (map :value)))))
      (finally
        (otel/close-gauge gauge)))
    (is (nil? (get (collect-metrics) "clj_money.test.gauge"))
        "No values are reported after the gauge is closed")))
