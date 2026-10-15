(ns clj-money.otel-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.otel :as otel]))

(def ^:private test-histogram
  (otel/histogram "clj_money.test.histogram"
                  :description "A histogram used in tests"
                  :unit "s"))

(deftest record-a-value-in-a-histogram
  (is (nil? (otel/record test-histogram
                         1.5
                         {"gen_ai.provider.name" "ollama"
                          "gen_ai.token.type" :input
                          "gen_ai.request.model" nil}))
      "The value is recorded without error"))
