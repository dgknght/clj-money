(ns clj-money.reports-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.trading :as trading]
            [clj-money.reports :as reports]
            [clj-money.test-helpers :refer [reset-db
                                            find-account
                                            find-accounts
                                            find-commodity
                                            find-commodities
                                            context-errors
                                            simplify-account-groups]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

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
                                   :amount 724M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :amount 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :amount 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :amount 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount 1001M}]}
                         {:transaction-date (t/local-date 2016 1 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount 725M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :amount 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :amount 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :amount 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount 1002M}]}
                         {:transaction-date (t/local-date 2016 2 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount 726M}
                                  {:action :debit
                                   :account-id "FIT"
                                   :amount 200M}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :amount 62M}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :amount 15M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount 1003M}]}
                         {:transaction-date (t/local-date 2016 2 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount 1004M}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount 1004M}]}

                         ; groceries
                         {:transaction-date (t/local-date 2016 1 3)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount 100M}]}
                         {:transaction-date (t/local-date 2016 1 10)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount 100M}]}
                         {:transaction-date (t/local-date 2016 1 17)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount 100M}]}
                         {:transaction-date (t/local-date 2016 1 24)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount 100M}]}
                         {:transaction-date (t/local-date 2016 1 31)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount 100M}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount 100M}]}
                                {:transaction-date (t/local-date 2016 2 7)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :amount 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :amount 101M}]}
                                {:transaction-date (t/local-date 2016 2 14)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :amount 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :amount 101M}]}
                                {:transaction-date (t/local-date 2016 2 21)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :amount 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :amount 101M}]}
                                {:transaction-date (t/local-date 2016 2 28)
                                :description "Kroger"
                                :items [{:action :debit
                                          :account-id "Groceries"
                                          :amount 101M}
                                        {:action :credit
                                          :account-id "Credit Card"
                                          :amount 101M}]}
                                ; rent
                                {:transaction-date (t/local-date 2016 1 4)
                                :description "Landlord"
                                :items [{:action :debit
                                          :account-id "Rent"
                                          :amount 700M}
                                        {:action :credit
                                          :account-id "Checking"
                                          :amount 700M}]}
                                {:transaction-date (t/local-date 2016 2 4)
                                :description "Landlord"
                                :items [{:action :debit
                                          :account-id "Rent"
                                          :amount 700M}
                                        {:action :credit
                                          :account-id "Checking"
                                          :amount 700M}]}]}))

(deftest create-an-income-statement
  (let [context (serialization/realize storage-spec report-context)
        actual (into [] (reports/income-statement storage-spec
                                                  (-> context :entities first :id)
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
    (is (= expected actual) "The report renders the corect data")))

(deftest create-a-balance-sheet-report
  (let [context (serialization/realize storage-spec report-context)
        actual (reports/balance-sheet storage-spec
                                      (-> context :entities first :id)
                                      (t/local-date 2016 1 31))
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
                   :value 249M
                   :style :data
                   :depth 0}
                  {:caption "Unrealized Gains"
                   :value 0M
                   :style :data
                   :depth 0}
                  {:caption "Liabilities + Equity"
                   :value 749M
                   :style :summary}]]
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
                                                    :amount 1000M}
                                                   {:action :debit
                                                    :account-id "IRA"
                                                    :amount 1000M}]}))
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
  (let [context (serialization/realize storage-spec commodities-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy storage-spec {:account-id (:id ira)
                                            :commodity-id (:id commodity)
                                            :shares 100M
                                            :value 500M
                                            :trade-date (t/local-date 2016 3 2)})
        report (reports/balance-sheet storage-spec
                                      (-> context :entities first :id)
                                      (t/local-date 2017 3 2))
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
    (is (= expected report) "The report contains the correct data")))

(deftest create-a-commodities-account-summary
  (let [context (serialization/realize storage-spec commodities-context)
        [ira
         lt-gains
         st-gains
         lt-losses
         st-losses] (find-accounts context "IRA"
                                           "LT Gains"
                                           "ST Gains"
                                           "LT Losses"
                                           "ST Losses")
        [aapl msft ge] (find-commodities context "AAPL"
                                                 "MSFT"
                                                 "GE")
        _ (trading/buy storage-spec {:account-id (:id ira)
                                     :commodity-id (:id ge)
                                     :shares 100M
                                     :value 1000M
                                     :trade-date (t/local-date 2015 1 1)})
        _ (trading/sell storage-spec {:account-id (:id ira)
                                      :commodity-id (:id ge)
                                      :shares 100M
                                      :value 2000M
                                      :trade-date (t/local-date 2015 12 20)
                                      :lt-capital-gains-account-id (:id lt-gains)
                                      :st-capital-gains-account-id (:id st-gains)
                                      :lt-capital-loss-account-id (:id lt-losses)
                                      :st-capital-loss-account-id (:id st-losses)})
        _ (trading/buy storage-spec {:account-id (:id ira)
                                     :commodity-id (:id aapl)
                                     :shares 50M
                                     :value 500
                                     :trade-date (t/local-date 2016 3 2)})
        _ (trading/buy storage-spec {:account-id (:id ira)
                                     :commodity-id (:id msft)
                                     :shares 50M
                                     :value 500M
                                     :trade-date (t/local-date 2016 3 2)})
        actual (reports/commodities-account-summary storage-spec
                                                    (:id ira)
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
    (if-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))
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
                                   :amount (bigdec 724)}
                                  {:action :debit
                                   :account-id "FIT"
                                   :amount (bigdec 200)}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :amount (bigdec 62)}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :amount (bigdec 15)}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount (bigdec 1001)}]}
                         {:transaction-date (t/local-date 2016 1 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount (bigdec 725)}
                                  {:action :debit
                                   :account-id "FIT"
                                   :amount (bigdec 200)}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :amount (bigdec 62)}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :amount (bigdec 15)}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount (bigdec 1002)}]}
                         {:transaction-date (t/local-date 2016 2 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount (bigdec 726)}
                                  {:action :debit
                                   :account-id "FIT"
                                   :amount (bigdec 200)}
                                  {:action :debit
                                   :account-id "Social Security"
                                   :amount (bigdec 62)}
                                  {:action :debit
                                   :account-id "Medicare"
                                   :amount (bigdec 15)}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount (bigdec 1003)}]}
                         {:transaction-date (t/local-date 2016 2 15)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount (bigdec 1004)}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount (bigdec 1004)}]}

                         ; groceries
                         {:transaction-date (t/local-date 2016 1 3)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount (bigdec 100)}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount (bigdec 100)}]}
                         {:transaction-date (t/local-date 2016 1 10)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount (bigdec 100)}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount (bigdec 100)}]}
                         {:transaction-date (t/local-date 2016 1 17)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount (bigdec 100)}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount (bigdec 100)}]}
                         {:transaction-date (t/local-date 2016 1 24)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount (bigdec 100)}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount (bigdec 100)}]}
                         {:transaction-date (t/local-date 2016 1 31)
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :amount (bigdec 100)}
                                  {:action :credit
                                   :account-id "Credit Card"
                                   :amount (bigdec 100)}]}
                                        {:transaction-date (t/local-date 2016 2 7)
                                        :description "Kroger"
                                        :items [{:action :debit
                                                  :account-id "Groceries"
                                                  :amount (bigdec 101)}
                                                {:action :credit
                                                  :account-id "Credit Card"
                                                  :amount (bigdec 101)}]}
                                        {:transaction-date (t/local-date 2016 2 14)
                                        :description "Kroger"
                                        :items [{:action :debit
                                                  :account-id "Groceries"
                                                  :amount (bigdec 101)}
                                                {:action :credit
                                                  :account-id "Credit Card"
                                                  :amount (bigdec 101)}]}
                                        {:transaction-date (t/local-date 2016 2 21)
                                        :description "Kroger"
                                        :items [{:action :debit
                                                  :account-id "Groceries"
                                                  :amount (bigdec 101)}
                                                {:action :credit
                                                  :account-id "Credit Card"
                                                  :amount (bigdec 101)}]}
                                        {:transaction-date (t/local-date 2016 2 28)
                                        :description "Kroger"
                                        :items [{:action :debit
                                                  :account-id "Groceries"
                                                  :amount (bigdec 101)}
                                                {:action :credit
                                                  :account-id "Credit Card"
                                                  :amount (bigdec 101)}]}
                                        ; rent
                                        {:transaction-date (t/local-date 2016 1 4)
                                        :description "Landlord"
                                        :items [{:action :debit
                                                  :account-id "Rent"
                                                  :amount (bigdec 700)}
                                                {:action :credit
                                                  :account-id "Checking"
                                                  :amount (bigdec 700)}]}
                                        {:transaction-date (t/local-date 2016 1 7)
                                        :description "Trunk Club"
                                        :items [{:action :debit
                                                  :account-id "Clothes"
                                                  :amount (bigdec 321)}
                                                {:action :credit
                                                  :account-id "Checking"
                                                  :amount (bigdec 700)}]}
                                        {:transaction-date (t/local-date 2016 2 4)
                                        :description "Landlord"
                                        :items [{:action :debit
                                                  :account-id "Rent"
                                                  :amount (bigdec 700)}
                                                {:action :credit
                                                  :account-id "Checking"
                                                  :amount (bigdec 700)}]}]}))

(deftest create-a-budget-report
  (let [context (serialization/realize storage-spec budget-report-context)
        actual (reports/budget storage-spec
                               (-> context :budgets first :id)
                               (t/local-date 2016 2 29))
        expected {:title "2016"
                  :items [{:caption "Income"
                           :style :header
                           :budget 4000M
                           :actual 4010M
                           :difference 10M
                           :percent-difference 0.0025M
                           :actual-per-period 2005M}
                          {:caption "Salary"
                           :style :data
                           :budget 4000M
                           :actual 4010M
                           :difference 10M
                           :percent-difference 0.0025M
                           :actual-per-period 2005M}
                          {:caption "Expense"
                           :style :header
                           :budget 3828M
                           :actual 3135M
                           :difference 693M
                           :percent-difference 0.1810344828M
                           :actual-per-period 1567.5M}
                          {:caption "Groceries"
                           :style :data
                           :budget 900M
                           :actual 904M
                           :difference -4M
                           :percent-difference -0.004444444444M
                           :actual-per-period 452M}
                          {:caption "Rent"
                           :style :data
                           :budget 1400M
                           :actual 1400M
                           :difference 0M
                           :percent-difference 0M
                           :actual-per-period 700M}
                          {:caption "Medicare"
                           :style :data
                           :budget 60M
                           :actual 45M
                           :difference 15M
                           :percent-difference 0.25M
                           :actual-per-period 22.5M}
                          {:caption "Social Security"
                           :style :data
                           :budget 268M
                           :actual 186M
                           :difference 82M
                           :percent-difference 0.3059701493M
                           :actual-per-period 93M}
                          {:caption "FIT"
                           :style :data
                           :budget 800M
                           :actual 600M
                           :difference 200M
                           :percent-difference 0.25M
                           :actual-per-period 300M}
                          {:caption "Dining"
                           :style :data
                           :budget 400M
                           :actual 0M
                           :difference 400M
                           :percent-difference 1M
                           :actual-per-period 0M}
                          {:caption "Net"
                           :style :summary
                           :budget 172M
                           :actual 875M
                           :difference 703M
                           :percent-difference 4.087209302M
                           :actual-per-period 437.50M}]}]
    (when (not= expected actual)
      (pprint {:diff (diff expected actual)}))
    (is (= expected actual) "The function products the correct data")))

; TODO
; create a budget report with nesting levels rolled up
; e.g.
; instead of:
; Entertainment/Drinks 100 110 -10
; Entertainment/Movies  80  75   5
; it would be:
; Entertainment        180 185  -5

(deftest create-a-budget-monitor
  (let [context (serialization/realize storage-spec budget-report-context)
        groceries (find-account context "Groceries")

        ; half-way through january
        actual (-> (reports/monitor storage-spec
                                groceries
                                {:as-of (t/local-date 2016 1 15)})
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

    (if-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))

    (is (= expected actual) "The correct information is returned")))

(deftest get-a-lot-report
  (let [context (serialization/realize storage-spec commodities-context)
        entity (-> context :entities first)
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
        p1 (trading/buy storage-spec {:trade-date (t/local-date 2017 1 15)
                                      :commodity-id (:id aapl)
                                      :account-id (:id ira)
                                      :shares 10M
                                      :value 100M})
        p2 (trading/buy storage-spec {:trade-date (t/local-date 2017 1 15)
                                      :commodity-id (:id msft)
                                      :account-id (:id ira)
                                      :shares 10M
                                      :value 100M})
        p3 (trading/buy storage-spec {:trade-date (t/local-date 2017 1 15)
                                      :commodity-id (:id ge)
                                      :account-id (:id ira)
                                      :shares 10M
                                      :value 100M})
        s1 (trading/sell storage-spec {:trade-date (t/local-date 2017 1 31)
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
                    (reports/lot-report storage-spec (:id ira)))
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
    (if (not= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))
    (is (= expected actual)
        "The report contains the correct data")))
