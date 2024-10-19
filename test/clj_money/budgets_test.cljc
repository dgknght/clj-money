(ns clj-money.budgets-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.util :as util]
            [clj-money.dates :as dates]
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

(deftest get-a-budget-end-date
  (testing "a monthly budget"
    (is (dates/equal? (t/local-date 2017 12 31)
                      (budgets/end-date #:budget{:period :month
                                                 :period-count 12
                                                 :start-date (t/local-date 2017 1 1)}))
        "It returns the last date of the last month"))
  (testing "a weekly budget"
    (is (dates/equal? (t/local-date 2017 2 4)
                      (budgets/end-date #:budget{:period :week
                                                 :period-count 5
                                                 :start-date (t/local-date 2017 1 1)}))
        "It returns the last date of the last week")))

(deftest get-the-index-of-the-period-containing-a-date
  (let [all-tests [{:period :month
                    :tests [{:date (t/local-date 2017 1 1)
                             :expected 0}
                            {:date (t/local-date 2017 1 31)
                             :expected 0}
                            {:date (t/local-date 2017 2 1)
                             :expected 1}
                            {:date (t/local-date 2017 2 28)
                             :expected 1}
                            {:date (t/local-date 2017 3 1)
                             :expected 2}
                            {:date (t/local-date 2017 12 31)
                             :expected 11}
                            {:date (t/local-date 2016 12 31)
                             :expected nil}
                            {:date (t/local-date 2018 1 1)
                             :expected nil}]}
                   {:period :week
                    :tests [{:date (t/local-date 2017 1 1)
                             :expected 0}
                            {:date (t/local-date 2017 1 7)
                             :expected 0}
                            {:date (t/local-date 2017 1 8)
                             :expected 1}
                            {:date (t/local-date 2017 3 25)
                             :expected 11}
                            {:date (t/local-date 2016 12 31)
                             :expected nil}
                            {:date (t/local-date 2017 3 26)
                             :expected nil}]}]
        budget #:budget{:start-date (t/local-date 2017 1 1)
                        :period-count 12}]
    (doseq [{:keys [period tests]} all-tests]
      (testing (util/format "period %s" period)
        (doseq [{:keys [date expected]} tests]
          (is (= expected (:index (budgets/period-containing
                                    (assoc budget :budget/period period)
                                    date)))
              (util/format "Given a budget starting on 1/1/2017, %s produces %s" date expected)))))

    (testing "monthly budget"
      (is (= 3 (:index (budgets/period-containing
                         (assoc budget :budget/period :month)
                         (t/local-date 2017 4 3))))
          "It returns the index of the period containing the date"))
    (testing "weekly budget"
      (is (= 3 (:index (budgets/period-containing
                         (assoc budget :budget/period :week)
                         (t/local-date 2017 1 25))))
          "It returns the index of the period containing the date"))))

(deftest calculate-a-percent-of-a-period
  (let [tests [{:description "the first day of a month"
                :budget #:budget{:period :month
                                 :period-count 12
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected 1/31}
               {:description "the 15th day of a month"
                :budget #:budget{:period :month
                                 :period-count 12
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 4 15)
                :expected 1/2}
               {:description "the last day of a month"
                :budget #:budget{:period :month
                                 :period-count 12
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 31)
                :expected 1/1}
               {:description "the first day of a week"
                :budget #:budget{:period :week
                                 :period-count 8
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected 1/7}
               {:description "the 4th day of a week"
                :budget #:budget{:period :week
                                 :period-count 8
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 4)
                :expected 4/7}
               {:description "the last day of a week"
                :budget #:budget{:period :week
                                 :period-count 8
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 7)
                :expected 1/1}
               {:description "the 1st day of a quarter"
                :budget #:budget{:period :quarter
                                 :period-count 4
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected 1/91}
               {:description "the 1st day of the 2nd month of a quarter"
                :budget #:budget{:period :quarter
                                 :period-count 4
                                 :start-date (t/local-date 2015 1 1)}
                :date (t/local-date 2015 2 1)
                :expected 16/45}
               {:description "the last day of a quarter"
                :budget #:budget{:period :quarter
                                 :period-count 4
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 3 31)
                :expected 1/1}]]
    (doseq [{:keys [description budget date expected]} tests]
      (testing description
        (is (= expected (budgets/percent-of-period budget date)))))))
