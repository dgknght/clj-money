(ns clj-money.budgets-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.budgets :as budgets]))

(deftest get-a-period-description
  (testing "monthly periods"
    (let [budget #:budget{:period :month
                          :start-date (t/local-date 2020 1 1)}]
      (is (= "Jan 2020" (budgets/period-description 0 budget)))
      (is (= "Feb 2020" (budgets/period-description 1 budget)))
      (is (= "Dec 2020" (budgets/period-description 11 budget)))))
  (testing "quarterly periods"
    (let [budget #:budget{:period :quarter
                          :start-date (t/local-date 2020 1 1)}]
      (is (= "Q1 2020" (budgets/period-description 0 budget)))
      (is (= "Q2 2020" (budgets/period-description 1 budget)))
      (is (= "Q3 2020" (budgets/period-description 2 budget)))
      (is (= "Q4 2020" (budgets/period-description 3 budget))))))

(def accounts
  (index-by :id
            [{:id :salary
              :account/path ["Salary"]
              :account/type :income}
             {:id :rent
              :account/path ["Rent"]
              :account/type :expense
              :account/user-tags #{:mandatory}}
             {:id :groceries
              :account/path ["Groceries"]
              :account/type :expense
              :account/user-tags #{:mandatory}}
             {:id :bonus
              :account/path ["Bonus"]
              :account/type :income}
             {:id :dining
              :account/path ["Dining"]
              :account/type :expense
              :account/user-tags #{:discretionary}}
             {:id :misc
              :account/path ["Misc"]
              :account/type :expense}]))

(def budget
  #:budget{:start-date (t/local-date 2020 1 1)
           :period :month
           :period-count 12
           :items [#:budget-item{:account {:id :salary}
                                 :periods (repeat 12 1000M)}
                   #:budget-item{:account {:id :bonus}
                                 :periods (concat (repeat 11 0M)
                                                  [1000M])}
                   #:budget-item{:account {:id :rent}
                                 :periods (repeat 12 500M)}
                   #:budget-item{:account {:id :groceries}
                                 :periods (repeat 12 250M)}
                   #:budget-item{:account {:id :dining}
                                 :periods (repeat 12 200M)}
                   #:budget-item{:account {:id :misc}
                                 :periods (repeat 12 5M)}]})

(def expected-rendering
  [#:budget-section{:caption "Income"
                    :total 13000M
                    :periods (concat (repeat 11 1000M)
                                     [2000M]) 
                    :items [#:budget-section{:caption "Bonus"
                                             :total 1000M
                                             :item #:budget-item{:account {:id :bonus}
                                                                 :periods (concat (repeat 11 0M)
                                                                                  [1000M])}}
                            #:budget-section{:caption "Salary"
                                             :total 12000M
                                             :item #:budget-item{:account {:id :salary}
                                                                 :periods (repeat 12 1000M)}}]}
   #:budget-section{:caption "Expenses"
                    :total 11460M
                    :periods (repeat 12 955M)
                    :items [#:budget-section{:caption "Dining"
                                             :total 2400M
                                             :item #:budget-item{:account {:id :dining}
                                                                 :periods (repeat 12 200M)}}
                            #:budget-section{:caption "Groceries"
                                             :total 3000M
                                             :item #:budget-item{:account {:id :groceries}
                                                                 :periods (repeat 12 250M)}}
                            #:budget-section{:caption "Misc"
                                             :total 60M
                                             :item #:budget-item{:account {:id :misc}
                                                                 :periods (repeat 12 5M)}}
                            #:budget-section{:caption "Rent"
                                             :total 6000M
                                             :item #:budget-item{:account {:id :rent}
                                                                 :periods (repeat 12 500M)}}]}
   #:budget-section{:caption "Net"
                    :total 1540M
                    :periods (concat (repeat 11 45M)
                                     [1045M])}])


#?(:clj (deftest render-a-budget
          (is (seq-of-maps-like?
                expected-rendering
                (budgets/render budget {:find-account accounts})))))

(def expected-categorized-rendering
  [#:budget-section{:caption "Income"
                    :total 13000M
                    :periods (concat (repeat 11 1000M)
                                     [2000M])
                    :items [#:budget-section{:caption "Bonus"
                                             :total 1000M
                                             :item #:budget-item{:account {:id :bonus}
                                                                 :periods (concat (repeat 11 0M)
                                                                                  [1000M])}}
                            #:budget-section{:caption "Salary"
                                             :total 12000M
                                             :item #:budget-item{:account {:id :salary}
                                                                 :periods (repeat 12 1000M)}}]}
   #:budget-section{:caption "Mandatory Expenses"
                    :total 9000M
                    :periods (repeat 12 750M)
                    :items [#:budget-section{:caption "Groceries"
                                             :total 3000M
                                             :item #:budget-item{:account {:id :groceries}
                                                                 :periods (repeat 12 250M)}}
                            #:budget-section{:caption "Rent"
                                             :total 6000M
                                             :item #:budget-item{:account {:id :rent}
                                                                 :periods (repeat 12 500M)}}]}
   #:budget-section{:caption "Available After Mandatory"
                    :total 4000M
                    :periods (concat (repeat 11 250M)
                                     [1250M])}
   #:budget-section{:caption "Discretionary Expenses"
                    :total 2400M
                    :periods (repeat 12 200M)
                    :items [#:budget-section{:caption "Dining"
                                             :total 2400M
                                             :item #:budget-item{:account {:id :dining}
                                                                 :periods (repeat 12 200M)}}]}
   #:budget-section{:caption "Available After Discretionary"
                    :total 1600M
                    :periods [50M 50M 50M 50M 50M 50M 50M 50M 50M 50M 50M 1050M]}
   #:budget-section{:caption "Uncategorized"
                    :total 60M
                    :periods (repeat 12 5M)
                    :items [#:budget-section{:caption "Misc"
                                             :total 60M
                                             :item #:budget-item{:account {:id :misc}
                                                                 :periods (repeat 12 5M)}}]}
   #:budget-section{:caption "Net"
                    :total 1540M
                    :periods (concat (repeat 11 45M)
                                     [1045M])}])

#?(:clj (deftest categorize-a-rendering-with-tags
          (is (seq-of-maps-like?
                expected-categorized-rendering
                (budgets/render budget {:find-account accounts
                                        :tags [:mandatory :discretionary]})))))

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
                :expected #?(:clj 1/31
                             :cljs 0.032)}
               {:description "the 15th day of a month"
                :budget #:budget{:period :month
                                 :period-count 12
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 4 15)
                :expected #?(:clj 1/2
                             :cljs 0.5)}
               {:description "the last day of a month"
                :budget #:budget{:period :month
                                 :period-count 12
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 31)
                :expected #?(:clj 1/1
                             :cljs 1.0)}
               {:description "the first day of a week"
                :budget #:budget{:period :week
                                 :period-count 8
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected #?(:clj 1/7
                             :cljs 0.143)}
               {:description "the 4th day of a week"
                :budget #:budget{:period :week
                                 :period-count 8
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 4)
                :expected #?(:clj 4/7
                             :cljs 0.571)}
               {:description "the last day of a week"
                :budget #:budget{:period :week
                                 :period-count 8
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 7)
                :expected #?(:clj 1/1
                             :cljs 1.0)}
               {:description "the 1st day of a quarter"
                :budget #:budget{:period :quarter
                                 :period-count 4
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected #?(:clj 1/91
                             :cljs 0.011)}
               {:description "the 1st day of the 2nd month of a quarter"
                :budget #:budget{:period :quarter
                                 :period-count 4
                                 :start-date (t/local-date 2015 1 1)}
                :date (t/local-date 2015 2 1)
                :expected #?(:clj 16/45
                             :cljs 0.360)}
               {:description "the last day of a quarter"
                :budget #:budget{:period :quarter
                                 :period-count 4
                                 :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 3 31)
                :expected #?(:clj 1/1
                             :cljs 1.0)}]]
    (doseq [{:keys [description budget date expected]} tests]
      (testing description
        (is (= expected (budgets/percent-of-period budget date)))))))

(deftest find-an-item-by-its-account
  (let [expected #:budget-item{:account {:id :groceries}
                               :periods [100M 100M]}
        actual (budgets/find-item-by-account #:budget{:items [#:budget-item{:account {:id :salary}
                                                                            :periods [1000M 1000M]}
                                                              #:budget-item{:account {:id :groceries}
                                                                            :periods [100M 100M]}]}
                                             {:id :groceries})]
    #?(:clj (is (comparable? expected actual))
       :cljs (is (dgknght.app-lib.test-assertions/comparable?
                   expected
                   actual)))))
