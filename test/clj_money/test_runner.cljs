(ns clj-money.test-runner
  (:require [clj-money.dates-test]
            [figwheel.main.testing :refer [run-tests-async]]))

(defn -main [& _args]
  (run-tests-async 5000))
