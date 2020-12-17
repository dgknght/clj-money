(ns clj-money.budgets-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-time.core :as t]
            [clj-money.budgets :as budgets]))

(deftest get-a-period-description
  (testing "monthly periods"
    (let [budget {:period :month
                  :start-date (t/local-date 2020 1 1)}]
      (is (= "Jan 2020" (budgets/period-description 0 budget)))
      (is (= "Feb 2020" (budgets/period-description 1 budget)))
      (is (= "Dec 2020" (budgets/period-description 11 budget)))))
  (testing "quarterly periods"
    (let [budget {:period :quarter
                  :start-date (t/local-date 2020 1 1)}]
      (is (= "Q1 2020" (budgets/period-description 0 budget)))
      (is (= "Q2 2020" (budgets/period-description 1 budget)))
      (is (= "Q3 2020" (budgets/period-description 2 budget)))
      (is (= "Q4 2020" (budgets/period-description 3 budget))))))

(def accounts
  {1 {:path "Salary"
      :type :income}
   2 {:path "Rent"
      :type :expense}
   3 {:path "Groceries"
      :type :expense}
   4 {:path "Bonus"
      :type :income}})

(deftest render-a-budget
  (let [budget {:start-date (t/local-date 2020 1 1)
                :period :month
                :period-count 12
                :items [{:account-id 1 ; Salary
                         :periods (repeat 12 1000M)}
                        {:account-id 4 ; Bonus
                         :periods (concat (repeat 11 0M)
                                          [1000M])}
                        {:account-id 2 ; Rent
                         :periods (repeat 12 500M)}
                        {:account-id 3 ; Groceries
                         :periods (repeat 12 250M)}]}
        expected [{:caption "Income"
                   :total 13000M
                   :periods [1000M 1000M 1000M 1000M 1000M 1000M 1000M 1000M 1000M 1000M 1000M 2000M]
                   :items [{:caption "Bonus"
                            :total 1000M
                            :item {:account-id 4
                                   :periods (concat (repeat 11 0M)
                                                    [1000M])}}
                           {:caption "Salary"
                            :total 12000M
                            :item {:account-id 1
                                   :periods (repeat 12 1000M)}}]}
                  {:caption "Expenses"
                   :total 9000M
                   :periods (repeat 12 750M)
                   :items [{:caption "Groceries"
                            :total 3000M
                            :item {:account-id 3 ; Groceries
                                   :periods (repeat 12 250M)}}
                           {:caption "Rent"
                            :total 6000M
                            :item {:account-id 2 ; Rent
                                   :periods (repeat 12 500M)}}]}
                  {:caption "Net"
                   :total 4000M
                   :periods [250M 250M 250M 250M 250M 250M 250M 250M 250M 250M 250M 1250M]}]
        actual (budgets/render budget
                               #(get-in accounts [%]))]
    (is (= expected actual))))
