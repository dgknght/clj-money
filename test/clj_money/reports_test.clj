(ns clj-money.reports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.core]
            [clj-money.util :refer [update-in-if]]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-commodity
                                            find-commodities]]
            [clj-money.factories.user-factory]
            [clj-money.trading :as trading]
            [clj-money.reports :as reports]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff]]))

(use-fixtures :each reset-db)

(defn- strip-account-ids
  [items]
  (map (fn [item]
         (-> item
             (update-in-if [:items] strip-account-ids)
             (dissoc item :id)))
       items))

(def ^:private base-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]})

(def ^:private report-context
  (merge base-context
         {:accounts [{:name "Checking"
                      :type :asset}
                     {:name "Credit Card"
                      :type :liability}
                     {:name "Salary"
                      :type :income}
                     {:name "Rent"
                      :type :expense}
                     {:name "Groceries"
                      :type :expense}
                     {:name "Taxes"
                      :type :expense}
                     {:name "FIT"
                      :type :expense
                      :parent-id "Taxes"}
                     {:name "Social Security"
                      :type :expense
                      :parent-id "Taxes"}
                     {:name "Medicare"
                      :type :expense
                      :parent-id "Taxes"}]
          :transactions [
                         ; salary
                         {:transaction-date (t/local-date 2016 1 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 724M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :quantity 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :quantity 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :quantity 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1001M}]}
                         {:transaction-date (t/local-date 2016 1 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 725M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :quantity 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :quantity 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :quantity 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1002M}]}
                         {:transaction-date (t/local-date 2016 2 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 726M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :quantity 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :quantity 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :quantity 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1003M}]}
                         {:transaction-date (t/local-date 2016 2 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 1004M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1004M}]}

                         ; groceries
                         {:transaction-date (t/local-date 2016 1 3)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :quantity 100M}]}
                         {:transaction-date (t/local-date 2016 1 10)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :quantity 100M}]}
                         {:transaction-date (t/local-date 2016 1 17)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :quantity 100M}]}
                         {:transaction-date (t/local-date 2016 1 24)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :quantity 100M}]}
                         {:transaction-date (t/local-date 2016 1 31)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :quantity 100M}]}
                                {:transaction-date (t/local-date 2016 2 7)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :quantity 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :quantity 101M}]}
                                {:transaction-date (t/local-date 2016 2 14)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :quantity 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :quantity 101M}]}
                                {:transaction-date (t/local-date 2016 2 21)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :quantity 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :quantity 101M}]}
                                {:transaction-date (t/local-date 2016 2 28)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :quantity 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :quantity 101M}]}
                                ; rent
                                {:transaction-date (t/local-date 2016 1 4)
                                :description "Landlord"
                                :items [{:action :debit
                                          :account-id "Rent"
                                          :quantity 700M}
                                        {:action :credit
                                          :account-id "Checking"
                                          :quantity 700M}]}
                                {:transaction-date (t/local-date 2016 2 4)
                                :description "Landlord"
                                :items [{:action :debit
                                          :account-id "Rent"
                                          :quantity 700M}
                                        {:action :credit
                                          :account-id "Checking"
                                          :quantity 700M}]}]}))

(deftest create-an-income-statement
  (let [context (realize report-context)
        entity (find-entity context "Personal")
        actual (strip-account-ids
                 (reports/income-statement entity
                                           (t/local-date 2016 1 1)
                                           (t/local-date 2016 1 31)))
        expected [{:caption "Income"
                   :value 2003M
                   :style :header}
                  {:caption "Salary"
                   :value 2003M
                   :style :data
                   :depth 0}
                  {:caption "Expense"
                   :value 1754M
                   :style :header}
                  {:caption "Groceries"
                   :value 500M
                   :style :data
                   :depth 0}
                  {:caption "Rent"
                   :value 700M
                   :style :data
                   :depth 0}
                  {:caption "Taxes"
                   :value 554M
                   :style :data
                   :depth 0}
                  {:caption "FIT"
                   :value 400M
                   :style :data
                   :depth 1}
                  {:caption "Medicare"
                   :value 30M
                   :style :data
                   :depth 1}
                  {:caption "Social Security"
                   :value 124M
                   :style :data
                   :depth 1}
                  {:caption "Net"
                   :value 249M
                   :style :summary}]]
    (pprint-diff expected actual)
    (is (= expected actual) "The report renders the corect data")))

(deftest create-a-balance-sheet-report
  (let [context (realize report-context)
        actual (strip-account-ids
                 (reports/balance-sheet (-> context :entities first)
                                        (t/local-date 2016 1 31)))
        expected [{:caption "Asset"
                   :value 749M
                   :style :header}
                  {:caption "Checking"
                   :value 749M
                   :style :data
                   :depth 0}
                  {:caption "Liability"
                   :value 500M
                   :style :header}
                  {:caption "Credit Card"
                   :value 500M
                   :style :data
                   :depth 0}
                  {:caption "Equity"
                   :value 249M
                   :style :header}
                  {:caption "Retained Earnings"
                   :value 249M ;  2,003 - 1,754
                   :style :data
                   :depth 0}
                  #_{:caption "Unrealized Gains"
                   :value 0M
                   :style :data
                   :depth 0}
                  {:caption "Liabilities + Equity"
                   :value 749M
                   :style :summary}]]
    (pprint-diff expected actual)
    (is (= expected actual) "The report data is correct")))

(def ^:private commodities-context
  (-> report-context
      (update-in [:accounts] #(concat % [{:name "IRA"
                                          :type :asset}
                                         {:name "LT Gains"
                                          :type :income}
                                         {:name "ST Gains"
                                          :type :income}
                                         {:name "LT Losses"
                                          :type :expense}
                                         {:name "ST Losses"
                                          :type :expense}]))
      (update-in [:transactions] #(conj % {:transaction-date (t/local-date 2016 1 2)
                                           :description "Retirement savings"
                                           :items [{:action :credit
                                                    :account-id "Checking"
                                                    :quantity 1000M}
                                                   {:action :debit
                                                    :account-id "IRA"
                                                    :quantity 1000M}]}))
      (update-in [:commodities] #(concat % [{:name "Apple, Inc."
                                             :symbol "AAPL"
                                             :type :stock
                                             :exchange :nasdaq}
                                            {:name "Microsoft Corp"
                                             :symbol "MSFT"
                                             :type :stock
                                             :exchange :nasdaq}
                                            {:name "General Electric Co."
                                             :symbol "GE"
                                             :type :stock
                                             :exchange :nyse}]))
      (assoc :prices [{:trade-date (t/local-date 2017 2 1)
                       :price 20M
                       :commodity-id "AAPL"}
                      {:trade-date (t/local-date 2017 2 1)
                       :price 5M
                       :commodity-id "MSFT"}])))

(deftest balance-sheet-report-with-commodities
  (let [context (realize commodities-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/buy {:account-id (:id ira)
                        :commodity-id (:id commodity)
                        :shares 100M
                        :value 500M
                        :trade-date (t/local-date 2016 3 2)})
        report (strip-account-ids
                 (reports/balance-sheet (-> context :entities first)
                                        (t/local-date 2017 3 2)))
        expected [{:caption "Asset"
                   :value 3279M
                   :style :header}
                  {:caption "Checking"
                   :value 779M
                   :style :data
                   :depth 0}
                  {:caption "IRA"
                   :value 2500M
                   :style :data
                   :depth 0}
                  {:caption "AAPL"
                   :value 2000M
                   :style :data
                   :depth 1}
                  {:caption "Liability"
                   :value 904M
                   :style :header}
                  {:caption "Credit Card"
                   :value 904M
                   :style :data
                   :depth 0}
                  {:caption "Equity"
                   :value 2375M
                   :style :header}
                  {:caption "Retained Earnings"
                   :value 875M
                   :style :data
                   :depth 0}
                  {:caption "Unrealized Gains"
                   :value 1500M
                   :style :data
                   :depth 0}
                  {:caption "Liabilities + Equity"
                   :value 3279M
                   :style :summary}]]
    (pprint-diff expected report)
    (is (= expected report) "The report contains the correct data")))

(def commodities-account-summary-context
  (assoc commodities-context
         :trades [{:type :buy
                   :account-id "IRA"
                   :commodity-id "GE"
                   :shares 100M
                   :value 1000M
                   :trade-date (t/local-date 2015 1 1)}
                  {:type :sell
                   :account-id "IRA"
                   :commodity-id "GE"
                   :shares 100M
                   :value 2000M
                   :trade-date (t/local-date 2015 12 20)
                   :lt-capital-gains-account-id "LT Gains"
                   :st-capital-gains-account-id "ST Gains"
                   :lt-capital-loss-account-id  "LT Losses"
                   :st-capital-loss-account-id "ST Losses"}
                  {:type :buy
                   :account-id "IRA"
                   :commodity-id "AAPL"
                   :shares 50M
                   :value 500M
                   :trade-date (t/local-date 2016 3 2)}
                  {:type :buy
                   :account-id "IRA"
                   :commodity-id "MSFT"
                   :shares 50M
                   :value 500M
                   :trade-date (t/local-date 2016 3 2)}]))

(deftest create-a-commodities-account-summary
  (let [context (realize commodities-account-summary-context)
        ira (find-account context "IRA")
        [aapl msft] (find-commodities context
                                      "AAPL"
                                      "MSFT")
        actual (reports/commodities-account-summary ira
                                                    (t/local-date 2017 3 2))
        expected [{:caption "Apple, Inc. (AAPL)"
                   :commodity-id (:id aapl)
                   :shares 50M
                   :price 20M
                   :cost 500M
                   :value 1000M
                   :gain 500M
                   :style :data}
                  {:caption "Microsoft Corp (MSFT)"
                   :commodity-id (:id msft)
                   :shares 50M
                   :price 5M
                   :cost 500M
                   :value 250M
                   :gain -250M
                   :style :data}
                  {:caption "Cash"
                   :style :data
                   :value 1000M}
                  {:caption "Total"
                   :cost 1000M
                   :value 2250M
                   :gain 250M
                   :style :summary}]]
    (pprint-diff expected actual)
    (is (= expected actual) "The report contains the correct data")))

(def ^:private budget-report-context
  (merge base-context
         {:accounts [{:name "Checking"
                      :type :asset}
                     {:name "Credit Card"
                      :type :liability}
                     {:name "Salary"
                      :type :income}
                     {:name "Dining"
                      :type :expense}
                     {:name "Clothes"
                      :type :expense}
                     {:name "Rent"
                      :type :expense}
                     {:name "Groceries"
                      :type :expense}
                     {:name "Taxes"
                      :type :expense}
                     {:name "FIT"
                      :type :expense
                      :parent-id "Taxes"}
                     {:name "Social Security"
                      :type :expense
                      :parent-id "Taxes"}
                     {:name "Medicare"
                      :type :expense
                      :parent-id "Taxes"}]
          :budgets [{:name "2016"
                     :start-date (t/local-date 2016 1 1)
                     :period :month
                     :period-count 12
                     :items [{:account-id "Salary"
                              :periods (repeat 12 2000M)}
                             {:account-id "FIT"
                              :periods (repeat 12 400M)}
                             {:account-id "Social Security"
                              :periods (repeat 12 134M)}
                             {:account-id "Medicare"
                              :periods (repeat 12 30M)}
                             {:account-id "Rent"
                              :periods (repeat 12 700M)}
                             {:account-id "Dining"
                              :periods (repeat 12 200M)}
                             {:account-id "Groceries"
                              :periods (repeat 12 450M)}]}]
          :transactions [
                         ; salary
                         {:transaction-date (t/local-date 2016 1 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 724M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :quantity 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :quantity 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :quantity 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1001M}]}
                         {:transaction-date (t/local-date 2016 1 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 725M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :quantity 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :quantity 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :quantity 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1002M}]}
                         {:transaction-date (t/local-date 2016 2 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 726M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :quantity 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :quantity 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :quantity 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1003M}]}
                         {:transaction-date (t/local-date 2016 2 15)
                          :description "Paycheck"
                          :quantity 1004M
                          :debit-account-id "Checking"
                          :credit-account-id "Salary"}

                         ; groceries
                         {:transaction-date (t/local-date 2016 1 3)
                          :description "Kroger"
                          :quantity 100M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 1 10)
                          :description "Kroger"
                          :quantity 100M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 1 17)
                          :description "Kroger"
                          :quantity 100M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 1 24)
                          :description "Kroger"
                          :quantity 100M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 1 31)
                          :description "Kroger"
                          :quantity 100M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 2 7)
                          :description "Kroger"
                          :quantity 101M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 2 14)
                          :description "Kroger"
                          :quantity 101M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 2 21)
                          :description "Kroger"
                          :quantity 101M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         {:transaction-date (t/local-date 2016 2 28)
                          :description "Kroger"
                          :quantity 101M
                          :debit-account-id "Groceries"
                          :credit-account-id "Credit Card"}
                         ; rent
                         {:transaction-date (t/local-date 2016 1 4)
                          :description "Landlord"
                          :quantity 700M
                          :debit-account-id "Rent"
                          :credit-account-id "Checking"}
                         {:transaction-date (t/local-date 2016 1 7)
                          :description "Trunk Club"
                          :quantity 321M
                          :debit-account-id "Clothes"
                          :credit-account-id "Checking"}
                         {:transaction-date (t/local-date 2016 2 4)
                          :description "Landlord"
                          :quantity 700M
                          :debit-account-id "Rent"
                          :credit-account-id "Checking"}]}))

(def ^:private  expected-budget
  {:title "2016: January to February"
   :items [{:caption "Income"
            :style :header
            :budget 4000M
            :actual 4010M
            :difference 10M
            :percent-difference 0.0025M
            :actual-per-period 2005M
            :items [{:caption "Salary"
                     :style :data
                     :depth 0
                     :budget 4000M
                     :actual 4010M
                     :difference 10M
                     :percent-difference 0.0025M
                     :actual-per-period 2005M}]}
           {:caption "Expense"
            :style :header
            :budget 3828M
            :actual 3456M
            :difference 372M
            :percent-difference 0.09717868339M
            :actual-per-period 1728M
            :items [{:caption "Clothes"
                     :style :data
                     :depth 0
                     :budget 0M
                     :actual 321M
                     :difference -321M
                     :percent-difference nil
                     :actual-per-period 160.50M}
                    {:caption "Groceries"
                     :style :data
                     :depth 0
                     :budget 900M
                     :actual 904M
                     :difference -4M
                     :percent-difference -0.0044444M
                     :actual-per-period 452M}
                    {:caption "Rent"
                     :style :data
                     :depth 0
                     :budget 1400M
                     :actual 1400M
                     :difference 0M
                     :percent-difference 0M
                     :actual-per-period 700M}
                    {:caption "Taxes"
                     :style :data
                     :depth 0
                     :budget 0M
                     :actual 0M
                     :difference 0M
                     :percent-difference nil
                     :actual-per-period 0M
                     :roll-up {:budget 1128M ; 60 + 268 + 800
                               :actual 831M ; 45 + 186 + 600
                               :difference 297M
                               :percent-difference 0.2632978723M
                               :actual-per-period 415.5M}}
                    {:caption "Taxes/Medicare"
                     :style :data
                     :depth 1
                     :budget 60M
                     :actual 45M
                     :difference 15M
                     :percent-difference 0.25M
                     :actual-per-period 22.5M}
                    {:caption "Taxes/Social Security"
                     :style :data
                     :depth 1
                     :budget 268M
                     :actual 186M
                     :difference 82M
                     :percent-difference 0.30597M
                     :actual-per-period 93M}
                    {:caption "Taxes/FIT"
                     :style :data
                     :depth 1
                     :budget 800M
                     :actual 600M
                     :difference 200M
                     :percent-difference 0.25M
                     :actual-per-period 300M}
                    {:caption "Dining"
                     :style :data
                     :depth 0
                     :budget 400M
                     :actual 0M
                     :difference 400M
                     :percent-difference 1M
                     :actual-per-period 0M}]}
           {:caption "Net"
            :style :summary
            :budget 172M
            :actual 554M
            :difference 382M
            :percent-difference 2.220930233M
            :actual-per-period 277M}]})

(deftest create-a-budget-report
  (let [context (realize budget-report-context)
        actual (update-in
                 (reports/budget (-> context :budgets first)
                                 {:as-of (t/local-date 2016 2 29)})
                 [:items]
                 strip-account-ids)]
    (pprint-diff expected-budget actual)
    (is (= expected-budget actual) "The function products the correct data")))

(deftest create-a-budget-monitor
  (let [context (realize budget-report-context)
        groceries (find-account context "Groceries")

        ; half-way through january
        actual (-> (reports/monitor groceries
                                    (t/local-date 2016 1 15))
                   (dissoc :account))
        expected {:caption "Groceries"
                  :period {:total-budget 450M
                           :prorated-budget 217.74M
                           :percentage 15/31
                           :actual 200M
                           :actual-percent 0.44444M}
                  :budget {:total-budget 5400M
                           :prorated-budget 221.31M
                           :percentage 15/366
                           :actual 200M
                           :actual-percent 0.037037M}}]
    (pprint-diff expected actual)
    (is (= expected actual) "The correct information is returned")))

(deftest get-a-lot-report
  (let [context (realize commodities-context)
        [ira
         lt-gains
         st-gains
         lt-losses
         st-losses] (find-accounts context "IRA"
                                           "LT Gains"
                                           "ST Gains"
                                           "LT Losses"
                                           "ST Losses")
        [aapl
         msft
         ge] (find-commodities context "AAPL"
                                       "MSFT"
                                       "GE")
        p1 (trading/buy {:trade-date (t/local-date 2017 1 15)
                         :commodity-id (:id aapl)
                         :account-id (:id ira)
                         :shares 10M
                         :value 100M})
        p2 (trading/buy {:trade-date (t/local-date 2017 1 15)
                         :commodity-id (:id msft)
                         :account-id (:id ira)
                         :shares 10M
                         :value 100M})
        p3 (trading/buy {:trade-date (t/local-date 2017 1 15)
                         :commodity-id (:id ge)
                         :account-id (:id ira)
                         :shares 10M
                         :value 100M})
        s1 (trading/sell {:trade-date (t/local-date 2017 1 31)
                          :commodity-id (:id aapl)
                          :account-id (:id ira)
                          :shares 5M
                          :value 55M
                          :lt-capital-gains-account-id (:id lt-gains)
                          :st-capital-gains-account-id (:id st-gains)
                          :lt-capital-loss-account-id (:id lt-losses)
                          :st-capital-loss-account-id (:id st-losses)})
        actual (map (fn [record]
                      (update-in record [:transactions] (fn [transactions]
                                                              (map #(dissoc % :lot-id)
                                                                   transactions))))
                    (reports/lot-report (:id ira)))
        expected [{:caption "Apple, Inc. (AAPL)"
                   :commodity-id (:id aapl)
                   :purchase-date (t/local-date 2017 1 15)
                   :shares-owned 5M
                   :purchase-price 10M
                   :cost 50M
                   :current-price 20M
                   :value 100M
                   :gain 50M
                   :transactions [{:transaction-date (t/local-date 2017 1 15)
                                   :id (-> p1 :transaction :id)
                                   :lot-action :buy
                                   :price 10M
                                   :shares 10M}
                                  {:transaction-date (t/local-date 2017 1 31)
                                   :id (-> s1 :transaction :id)
                                   :lot-action :sell
                                   :price 11M
                                   :shares 5M}]}
                  {:caption "General Electric Co. (GE)"
                   :commodity-id (:id ge)
                   :purchase-date (t/local-date 2017 1 15)
                   :shares-owned 10M
                   :purchase-price 10M
                   :cost 100M
                   :value 100M
                   :current-price 10M
                   :gain 0M
                   :transactions [{:transaction-date (t/local-date 2017 1 15)
                                   :id (-> p3 :transaction :id)
                                   :lot-action :buy
                                   :shares 10M
                                   :price 10M}]}
                  {:caption "Microsoft Corp (MSFT)"
                   :commodity-id (:id msft)
                   :purchase-date (t/local-date 2017 1 15)
                   :shares-owned 10M
                   :purchase-price 10M
                   :cost 100M
                   :value 50M
                   :current-price 5M
                   :gain -50M
                   :transactions [{:transaction-date (t/local-date 2017 1 15)
                                   :id (-> p2 :transaction :id)
                                   :lot-action :buy
                                   :shares 10M
                                   :price 10M}]}]]
    (when (not= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))
    (is (= expected actual)
        "The report contains the correct data")))

(def ^:private portfolio-context
  (-> basic-context
      (update-in [:accounts] concat [{:name "IRA"
                                      :type :asset
                                      :tags #{:trading}
                                      :entity-id "Personal"}
                                     {:name "401k"
                                      :type :asset
                                      :tags #{:trading}
                                      :entity-id "Personal"}])
      (update-in [:commodities] concat [{:name "Apple, Inc."
                                         :symbol "AAPL"
                                         :type :stock
                                         :exchange :nasdaq}
                                        {:name "Microsoft, Inc."
                                         :symbol "MSFT"
                                         :type :stock
                                         :exchange :nasdaq}
                                        {:name "Alphabet, Inc."
                                         :symbol "GOOG"
                                         :type :stock
                                         :exchange :nasdaq}])
      (assoc :transactions [{:transaction-date (t/local-date 2015 1 1)
                             :description "Begining balance"
                             :quantity 10000M
                             :debit-account-id "401k"
                             :credit-account-id "Opening Balances"}
                            {:transaction-date (t/local-date 2015 1 1)
                             :description "Begining balance"
                             :quantity 10000M
                             :debit-account-id "IRA"
                             :credit-account-id "Opening Balances"}]
             :trades [{:trade-date (t/local-date 2015 2 1)
                       :type :purchase
                       :account-id "IRA"
                       :commodity-id "AAPL"
                       :shares 200M
                       :value  2000M}
                      {:trade-date (t/local-date 2015 2 1)
                       :type :purchase
                       :account-id "401k"
                       :commodity-id "MSFT"
                       :shares 400M
                       :value  2000M}])))

(def ^:private expected-portfolio-by-account
  [{:caption "401k"
    :style :header
    :cost-basis 10000M
    :current-value 10000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "Cash"
    :style :subheader
    :current-value 8000M
    :cost-basis 8000M
    :gain-loss 0M}
   {:caption "Microsoft, Inc."
    :style :subheader
    :shares-owned 400M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "2/1/2015"
    :style :data
    :shares-purchased 400M
    :shares-owned 400M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "IRA"
    :style :header
    :cost-basis 10000M
    :current-value 10000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "Cash"
    :style :subheader
    :current-value 8000M
    :cost-basis 8000M
    :gain-loss 0M}
   {:caption "Apple, Inc."
    :style :subheader
    :shares-owned 200M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "2/1/2015"
    :style :data
    :shares-purchased 200M
    :shares-owned 200M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "Total"
    :style :summary
    :current-value 20000M
    :cost-basis 20000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}])

(def ^:private expected-portfolio-by-commodity
  [{:caption "Cash"
    :style :subheader
    :shares-owned 16000M
    :cost-basis 16000M
    :current-value 16000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "Apple, Inc."
    :style :subheader
    :shares-owned 200M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "2/1/2015"
    :style :data
    :shares-purchased 200M
    :shares-owned 200M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "Microsoft, Inc."
    :style :subheader
    :shares-owned 400M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "2/1/2015"
    :style :data
    :shares-purchased 400M
    :shares-owned 400M
    :cost-basis 2000M
    :current-value 2000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}
   {:caption "Total"
    :style :summary
    :cost-basis 20000M
    :current-value 20000M
    :gain-loss 0M
    :gain-loss-percent 0.0M}])

(deftest get-a-portfolio-report-by-account
  (let [ctx (realize portfolio-context)
        entity (find-entity ctx "Personal")
        actual (map #(select-keys % [:caption
                                     :style
                                     :shares-purchased
                                     :shares-owned
                                     :cost-basis
                                     :current-value
                                     :gain-loss
                                     :gain-loss-percent])
                    (reports/portfolio (:id entity) {:aggregate :by-account}))]
    (pprint-diff expected-portfolio-by-account actual)
    (is (= expected-portfolio-by-account actual)
        "The correct report data is generated")))

(deftest get-a-portfolio-report-by-commodity
  (let [ctx (realize portfolio-context)
        entity (find-entity ctx "Personal")
        actual (map #(select-keys % [:caption
                                     :style
                                     :shares-purchased
                                     :shares-owned
                                     :cost-basis
                                     :current-value
                                     :gain-loss
                                     :gain-loss-percent])
                    (reports/portfolio (:id entity) {:aggregate :by-commodity}))]
    (pprint-diff expected-portfolio-by-commodity actual)
    (is (= expected-portfolio-by-commodity actual)
        "The correct report data is generated")))
