(ns clj-money.models.reports-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.models.reports :as reports]
            [clj-money.test-helpers :refer [reset-db
                                            simplify-account-groups
                                            ->budget-item-periods]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def income-statement-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
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
                             :amount 700M}]}]})

(deftest create-an-income-statement
  (let [context (serialization/realize storage-spec income-statement-context)
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
  (let [context (serialization/realize storage-spec income-statement-context)
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
                   :depth 0}]]
    (is (= expected actual) "The rpoert renders the correct data")))

(def budget-report-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
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
                       :periods  (->budget-item-periods (repeat 12 2000M))}
                      {:account-id "FIT"
                       :periods  (->budget-item-periods (repeat 12 400M))}
                      {:account-id "Social Security"
                       :periods  (->budget-item-periods (repeat 12 134M))}
                      {:account-id "Medicare"
                       :periods  (->budget-item-periods (repeat 12 30M))}
                      {:account-id "Rent"
                       :periods  (->budget-item-periods (repeat 12 700M))}
                      {:account-id "Dining"
                       :periods  (->budget-item-periods (repeat 12 200M))}
                      {:account-id "Groceries"
                       :periods (->budget-item-periods (repeat 12 450M))}]}]
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
                             :amount (bigdec 700)}]}]})

(deftest create-a-budget-report
  (let [context (serialization/realize storage-spec budget-report-context)
        actual (reports/budget storage-spec (-> context :budgets first :id) (t/local-date 2016 2 29))
        expected {:title "2016"
                  :items [{:caption "Income"
                           :style :header
                           :budget 4000M
                           :actual 4010M
                           :difference 10M
                           :percent-difference 0.0025M
                           :actual-per-month 2005M}
                          {:caption "Salary"
                           :style :data
                           :budget 4000M
                           :actual 4010M
                           :difference 10M
                           :percent-difference 0.0025M
                           :actual-per-month 2005M}
                          {:caption "Expense"
                           :style :header
                           :budget 0M
                           :actual 0M
                           :difference 0M
                           :percent-difference 0M
                           :actual-per-month 0M}
                          {:caption "Groceries"
                           :style :data
                           :budget 900M
                           :actual 904M
                           :difference -4M
                           :percent-difference 0.0044M
                           :actual-per-month 2005M}]}]
    
    #_(pprint {:expected expected
             :actual actual
             :diff (diff expected actual)})
    
    (is (= expected actual) "The function products the correct data")))

; TODO
; create a budget report with nesting levels rolled up
; e.g.
; instead of:
; Entertainment/Drinks 100 110 -10
; Entertainment/Movies  80  75   5
; it would be:
; Entertainment        180 185  -5
