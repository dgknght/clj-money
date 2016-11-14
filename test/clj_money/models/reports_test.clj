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
                            :account-id "Checking"
                            :amount (bigdec 100)}]}
                  {:transaction-date (t/local-date 2016 1 10)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 100)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 100)}]}
                  {:transaction-date (t/local-date 2016 1 17)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 100)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 100)}]}
                  {:transaction-date (t/local-date 2016 1 24)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 100)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 100)}]}
                  {:transaction-date (t/local-date 2016 1 31)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 100)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 100)}]}
                  {:transaction-date (t/local-date 2016 2 7)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 101)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 101)}]}
                  {:transaction-date (t/local-date 2016 2 14)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 101)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 101)}]}
                  {:transaction-date (t/local-date 2016 2 21)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 101)}
                           {:action :credit
                            :account-id "Checking"
                            :amount (bigdec 101)}]}
                  {:transaction-date (t/local-date 2016 2 28)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount (bigdec 101)}
                           {:action :credit
                            :account-id "Checking"
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
                  {:transaction-date (t/local-date 2016 2 4)
                   :description "Landlord"
                   :items [{:action :debit
                             :account-id "Rent"
                             :amount (bigdec 700)}
                           {:action :credit
                             :account-id "Checking"
                             :amount (bigdec 700)}]}]})

(deftest create-an-income-statement
  (let [context (serialization/realize storage-spec income-statement-context)
        actual (reports/income-statement storage-spec
                                         (-> context :entities first :id)
                                         (t/local-date 2016 1 1)
                                         (t/local-date 2016 1 31))
        expected [{:caption "Income"
                   :value (bigdec 2003)
                   :style :header}
                  {:caption "Salary"
                   :value (bigdec 2003)
                   :style :data
                   :depth 0}
                  {:caption "Expense"
                   :value (bigdec 1754)
                   :style :header}
                  {:caption "Groceries"
                   :value (bigdec 500)
                   :style :data
                   :depth 0}
                  {:caption "Rent"
                   :value (bigdec 700)
                   :style :data
                   :depth 0}
                  {:caption "Taxes"
                   :value (bigdec 554)
                   :style :data
                   :depth 0}
                  {:caption "FIT"
                   :value (bigdec 400)
                   :style :data
                   :depth 1}
                  {:caption "Medicare"
                   :value (bigdec 30)
                   :style :data
                   :depth 1}
                  {:caption "Social Security"
                   :value (bigdec 124)
                   :style :data
                   :depth 1}
                  {:caption "Net"
                   :value (bigdec 249)
                   :style :summary}]]
    (is (= expected actual) "The report renders the corect data")))

(deftest create-a-balance-sheet-report
  (let [context (serialization/realize storage-spec income-statement-context)
        actual (reports/balance-sheet storage-spec
                                      (-> context :entities first :id)
                                      (t/local-date 2016 1 31))
        expected [{:caption "Asset"
                   :value (bigdec 249)
                   :style :header}
                  {:caption "Checking"
                   :value (bigdec 249)
                   :style :data
                   :depth 0}
                  {:caption "Liability"
                   :value (bigdec 0)
                   :style :header}
                  {:caption "Equity"
                   :value (bigdec 249)
                   :style :header}
                  {:caption "Retained Earnings"
                   :value (bigdec 249)
                   :style :data
                   :depth 0}]]
    (is (= expected actual) "The rpoert renders the correct data")))
