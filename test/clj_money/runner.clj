(ns clj-money.runner
  (:require [clojure.pprint :refer [pprint]]
            [eftest.runner :refer [find-tests run-tests]]))

(def ^:private cljc-tests
  ["test/clj_money/accounts_test.cljc"
   "test/clj_money/budgets_test.cljc"
   "test/clj_money/commodities_test.cljc"
   "test/clj_money/comparatives_test.cljc"
   "test/clj_money/dates_test.cljc"
   "test/clj_money/decimal_test.cljc"])

(defn eftest
  [& args]
  (pprint {::eftest args
           ::processors (.availableProcessors (Runtime/getRuntime))})
  (run-tests (find-tests "test/clj_money/entities/accounts_test.clj")))
