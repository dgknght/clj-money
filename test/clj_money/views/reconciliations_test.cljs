(ns clj-money.views.reconciliations-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.views.reconciliations :as recs]))

(deftest get-the-path-to-the-reconciliations
  (is (= "/reconciliations"
         (recs/list-path))
      "Without an account, the path lists all reconciliations")
  (is (= "/reconciliations?account-id=101%3Aaccount"
         (recs/list-path {:id "101:account"}))
      "With an account, the path identifies it"))
