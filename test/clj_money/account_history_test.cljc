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
          :period [1 :month]
          :include-balance true
          :include-value true}
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
  "Returns a summary for an account that tracks the default commodity, for
  which the values are the same as the quantities"
  ([start-dates quantities]
   (summary start-dates quantities (repeat "0")))
  ([start-dates quantities balances]
   (map (fn [start-date quantity balance]
          {:start-date start-date
           :quantity (d quantity)
           :value (d quantity)
           :balance (d balance)
           :balance-value (d balance)})
        start-dates
        quantities
        balances)))

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

(defn- two-account-config
  [opts]
  (hist/chart-config [{:account groceries
                       :summary (summary quarters ["100" "200.5" "0"])}
                      {:account dining
                       :summary (summary quarters ["50" "0" "25.25"])}]
                     (assoc opts
                            :period-type :quarter
                            :show-values? true)))

(deftest get-the-config-for-a-stacked-bar-chart
  (is (= {:plugins {:datalabels {:display true
                                 :anchor "center"
                                 :align "center"}}
          :scales {:x {:stacked true}
                   :y {:stacked true}}}
         (:options (two-account-config {:style :bar
                                        :stacked? true})))
      "Both axes are stacked and the values are labeled inside their segments"))

(deftest only-a-bar-chart-is-stacked
  (is (= {:plugins {:datalabels {:display true
                                 :anchor "end"
                                 :align "end"}}}
         (:options (two-account-config {:style :line
                                        :stacked? true})))
      "A line chart ignores the stacked option"))

(deftest get-the-config-for-a-chart-of-balances
  (is (= [{:label "Groceries"
           :data [1100.0 1300.5 1300.5]}]
         (get-in (hist/chart-config [{:account groceries
                                      :summary (summary quarters
                                                        ["100" "200.5" "0"]
                                                        ["1100" "1300.5" "1300.5"])}]
                                    {:period-type :quarter
                                     :measure :balance
                                     :style :line})
                 [:data :datasets]))
      "The balance at the end of each period is charted instead of the activity"))

(def ^:private fund
  {:id 103
   :account/path ["IRA" "FND"]})

(def ^:private fund-summary
  [{:start-date (t/local-date 2026 1 1)
    :quantity (d "10")
    :value (d "50")
    :balance (d "10")
    :balance-value nil}
   {:start-date (t/local-date 2026 4 1)
    :quantity (d "5")
    :value (d "30")
    :balance (d "15")
    :balance-value (d "105")}])

(defn- fund-data
  [opts]
  (get-in (hist/chart-config [{:account fund
                               :summary fund-summary}]
                             (assoc opts
                                    :period-type :quarter
                                    :style :line))
          [:data :datasets 0 :data]))

(deftest chart-a-commodity-account-by-value-or-quantity
  (testing "activity"
    (is (= [50.0 30.0]
           (fund-data {:measure :activity}))
        "The value is charted by default")
    (is (= [10.0 5.0]
           (fund-data {:measure :activity
                       :unit :quantity}))
        "The shares are charted when quantity is selected"))
  (testing "balance"
    (is (= [nil 105.0]
           (fund-data {:measure :balance
                       :unit :value}))
        "A period with no price is left blank")
    (is (= [10.0 15.0]
           (fund-data {:measure :balance
                       :unit :quantity})))))

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
