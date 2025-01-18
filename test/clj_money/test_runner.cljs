(ns clj-money.test-runner
  (:require [clj-money.dates-test]
            [clj-money.accounts-test]
            [clj-money.budgets-test]
            [clj-money.util-test]
            [clj-money.transactions-test]
            [figwheel.main.testing :refer [run-tests-async]]))

(defn -main [& _args]
  (run-tests-async 5000))
