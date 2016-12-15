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
                                            simplify-account-groups]]))

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
