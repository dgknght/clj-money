(ns clj-money.account-history-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.decimal :refer [d]]
            [clj-money.account-history :as hist]))

(def ^:private groceries
  {:id 101
   :account/path ["Groceries"]})

(def ^:private dining
  {:id 102
   :account/path ["Entertainment" "Dining"]})

(defn- criteria-start
  [start-date period-type]
  (get-in (hist/criteria groceries {:start-date start-date
                                    :end-date (t/local-date 2026 12 31)
                                    :period-type period-type})
          [:transaction/transaction-date 0]))

(deftest get-the-criteria-for-summarizing-an-account
  (is (= {:transaction/transaction-date [(t/local-date 2026 1 1)
                                         (t/local-date 2026 7 1)]
          :transaction-item/account groceries
          :period [1 :month]}
         (hist/criteria groceries {:start-date (t/local-date 2026 1 1)
                                   :end-date (t/local-date 2026 6 30)
                                   :period-type :month}))
      "The end of the range is the day after the end date, since it's exclusive"))

(deftest align-the-start-date-with-the-start-of-its-period
  (testing "year"
    (is (= (t/local-date 2026 1 1)
           (criteria-start (t/local-date 2026 5 15) :year))))
  (testing "quarter"
    (is (= (t/local-date 2026 4 1)
           (criteria-start (t/local-date 2026 5 15) :quarter)))
    (is (= (t/local-date 2026 10 1)
           (criteria-start (t/local-date 2026 12 31) :quarter))))
  (testing "month"
    (is (= (t/local-date 2026 5 1)
           (criteria-start (t/local-date 2026 5 15) :month))))
  (testing "week"
    (is (= (t/local-date 2026 5 11)
           (criteria-start (t/local-date 2026 5 15) :week))
        "The week starts on Monday")))

(defn- summary
  [start-dates quantities]
  (map (fn [start-date quantity]
         {:start-date start-date
          :quantity (d quantity)})
       start-dates
       quantities))

(def ^:private quarters
  [(t/local-date 2026 1 1)
   (t/local-date 2026 4 1)
   (t/local-date 2026 7 1)])

(deftest get-the-config-for-a-chart
  (is (= {:type :bar
          :data {:labels ["Q1 2026" "Q2 2026" "Q3 2026"]
                 :datasets [{:label "Groceries"
                             :data [100.0 200.5 0.0]}
                            {:label "Entertainment/Dining"
                             :data [50.0 0.0 25.25]}]}
          :options {:plugins {:datalabels {:display true
                                           :anchor "end"
                                           :align "end"}}}}
         (hist/chart-config [{:account groceries
                              :summary (summary quarters ["100" "200.5" "0"])}
                             {:account dining
                              :summary (summary quarters ["50" "0" "25.25"])}]
                            {:period-type :quarter
                             :style :bar
                             :show-values? true}))))

(defn- labels
  [period-type start-dates]
  (get-in (hist/chart-config [{:account groceries
                               :summary (summary start-dates (repeat "0"))}]
                             {:period-type period-type
                              :style :line})
          [:data :labels]))

(deftest label-each-period
  (testing "year"
    (is (= ["2025" "2026"]
           (labels :year [(t/local-date 2025 1 1)
                          (t/local-date 2026 1 1)]))))
  (testing "month"
    (is (= ["Dec 2025" "Jan 2026"]
           (labels :month [(t/local-date 2025 12 1)
                           (t/local-date 2026 1 1)]))))
  (testing "week"
    (is (= ["10/26/2026"]
           (labels :week [(t/local-date 2026 10 26)])))))
