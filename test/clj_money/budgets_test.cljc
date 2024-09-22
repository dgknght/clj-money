(ns clj-money.budgets-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
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
  {1 {:path ["Salary"]
      :type :income}
   2 {:path ["Rent"]
      :type :expense
      :user-tags #{:mandatory}
      }
   3 {:path ["Groceries"]
      :type :expense
      :user-tags #{:mandatory}}
   4 {:path ["Bonus"]
      :type :income}
   5 {:path ["Dining"]
      :type :expense
      :user-tags #{:discretionary}}
   6 {:path ["Misc"]
      :type :expense}})

(def budget
  {:start-date (t/local-date 2020 1 1)
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
            :periods (repeat 12 250M)}
           {:account-id 5 ; Dining
            :periods (repeat 12 200M)}
           {:account-id 6 ; Misc
            :periods (repeat 12 5M)}]})

(def expected-rendering
  [{:caption "Income"
    :total 13000M
    :periods (concat (repeat 11 1000M)
                     [2000M]) 
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
    :total 11460M
    :periods (repeat 12 955M)
    :items [{:caption "Dining"
             :total 2400M
             :item {:account-id 5
                    :periods (repeat 12 200M)}}
            {:caption "Groceries"
             :total 3000M
             :item {:account-id 3
                    :periods (repeat 12 250M)}}
            {:caption "Misc"
             :total 60M
             :item {:account-id 6
                    :periods (repeat 12 5M)}}
            {:caption "Rent"
             :total 6000M
             :item {:account-id 2
                    :periods (repeat 12 500M)}}]}
   {:caption "Net"
    :total 1540M
    :periods (concat (repeat 11 45M)
                     [1045M])}])

(deftest render-a-budget
  (is (= expected-rendering
         (budgets/render budget {:find-account accounts}))))

(def expected-categorized-rendering
  [{:caption "Income"
    :total 13000M
    :periods (concat (repeat 11 1000M)
                     [2000M])
    :items [{:caption "Bonus"
             :total 1000M
             :item {:account-id 4
                    :periods (concat (repeat 11 0M)
                                     [1000M])}}
            {:caption "Salary"
             :total 12000M
             :item {:account-id 1
                    :periods (repeat 12 1000M)}}]}
   {:caption "Mandatory Expenses"
    :total 9000M
    :periods (repeat 12 750M)
    :items [{:caption "Groceries"
             :total 3000M
             :item {:account-id 3
                    :periods (repeat 12 250M)}}
            {:caption "Rent"
             :total 6000M
             :item {:account-id 2
                    :periods (repeat 12 500M)}}]}
   {:caption "Available After Mandatory"
    :total 4000M
    :periods (concat (repeat 11 250M)
                     [1250M])}
   {:caption "Discretionary Expenses"
    :total 2400M
    :periods (repeat 12 200M)
    :items [{:caption "Dining"
             :total 2400M
             :item {:account-id 5
                    :periods (repeat 12 200M)}}]}
   {:caption "Available After Discretionary"
    :total 1600M
    :periods [50M 50M 50M 50M 50M 50M 50M 50M 50M 50M 50M 1050M]}
   {:caption "Uncategorized"
    :total 60M
    :periods (repeat 12 5M)
    :items [{:caption "Misc"
             :total 60M
             :item {:account-id 6
                    :periods (repeat 12 5M)}}]}
   {:caption "Net"
    :total 1540M
    :periods (concat (repeat 11 45M)
                     [1045M])}])

(deftest categorize-a-rendering-with-tags
  (is (= expected-categorized-rendering
         (budgets/render budget {:find-account accounts
                                 :tags [:mandatory :discretionary]}))))
