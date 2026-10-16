(ns clj-money.otel-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.otel :as otel]))

(def ^:private test-histogram
  (otel/histogram "clj_money.test.histogram"
                  :description "A histogram used in tests"
                  :unit "s"
                  :buckets otel/duration-buckets))

(deftest record-a-value-in-a-histogram
  (is (nil? (otel/record test-histogram
                         1.5
                         {"gen_ai.provider.name" "ollama"
                          "gen_ai.token.type" :input
                          "gen_ai.request.model" nil}))
      "The value is recorded without error"))

(deftest observe-a-value-with-a-gauge
  (let [gauge (otel/gauge "clj_money.test.gauge"
                          (constantly [[3 {"queue" "test"}]])
                          :description "A gauge used in tests"
                          :unit "{message}")]
    (is (some? gauge) "The gauge is returned")
    (is (nil? (otel/close-gauge gauge))
        "The gauge is closed without error")))
