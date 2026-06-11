(ns clj-money.test-runner
  (:require [cljs.test :as t]
            [clj-money.dates-test]
            [clj-money.accounts-test]
            [clj-money.budgets-test]
            [clj-money.util-test]
            [clj-money.transactions-test]
            [clj-money.scheduled-transactions-test]
            [clj-money.routes-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (set! (.-exitCode js/process) (if (t/successful? m) 0 1)))

(defn -main [& _args]
  (t/run-tests
    'clj-money.routes-test
    'clj-money.dates-test
    'clj-money.accounts-test
    'clj-money.budgets-test
    'clj-money.util-test
    'clj-money.transactions-test
    'clj-money.scheduled-transactions-test))
