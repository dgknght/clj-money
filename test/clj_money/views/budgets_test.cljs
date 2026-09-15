(ns clj-money.views.budgets-test
  (:require [cljs.test :refer [deftest is testing]]
            [clj-money.views.budgets :as budgets]))

(deftest fill-periods-from-applies-the-value-to-later-periods
  (testing "later periods are overwritten, earlier ones are untouched"
    (is (= [10M 10M 10M 10M]
           (budgets/fill-periods-from [10M 20M 30M 40M] 0))))
  (testing "applying from a middle index preserves earlier values"
    (is (= [10M 20M 20M 20M]
           (budgets/fill-periods-from [10M 20M 30M 40M] 1))))
  (testing "applying from the last index changes nothing"
    (is (= [10M 20M 30M 40M]
           (budgets/fill-periods-from [10M 20M 30M 40M] 3)))))
