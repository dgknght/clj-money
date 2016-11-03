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
               :type :expense}]
   :transactions [
                  ; salary
                  {:transaction-date (t/local-date 2016 1 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount (bigdec 1000)}
                           {:action :credit
                            :account-id "Salary"
                            :amount (bigdec 1000)}]}
                  {:transaction-date (t/local-date 2016 1 15)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount (bigdec 1000)}
                           {:action :credit
                            :account-id "Salary"
                            :amount (bigdec 1000)}]}

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
                  ; rent
                  {:transaction-date (t/local-date 2016 1 4)
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
                   :value (bigdec 2000)
                   :style :header}
                  {:caption "Salary"
                   :value (bigdec 2000)
                   :style :data
                   :depth 0}
                  {:caption "Expense"
                   :value (bigdec 1200)
                   :style :header}
                  {:caption "Groceries"
                   :value (bigdec 500)
                   :style :data
                   :depth 0}
                  {:caption "Rent"
                   :value (bigdec 700)
                   :style :data
                   :depth 0}
                  {:caption "Net"
                   :value (bigdec 800)
                   :style :summary}]]

    (pprint {:expected expected
             :actual actual
             :diff (diff expected actual)})
    
    (is (= expected actual) "The report renders the corect data")))
