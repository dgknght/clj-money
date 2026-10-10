(ns clj-money.ingestion.ollama-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.ingestion.ollama]))

(def ^:private handle-success-response
  #'clj-money.ingestion.ollama/handle-success-response)

(deftest amounts-in-the-response-are-read-as-decimals
  (let [result (handle-success-response
                 {:response "{\"total\": 38.97, \"line_items\": [{\"amount\": 30.30}]}"}
                 {:options {:num_ctx 4096}})]
    (is (= 38.97M (:total result))
        "The total is a decimal")
    (is (decimal? (get-in result [:line-items 0 :amount]))
        "The line item amounts are decimals")))
