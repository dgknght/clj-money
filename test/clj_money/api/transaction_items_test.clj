(ns clj-money.api.transaction-items-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.api.test-helper :refer [deftest-create
                                               deftest-delete
                                               deftest-update
                                               deftest-list]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff
                                            with-authentication
                                            find-account
                                            find-user]]
            [clj-money.api.transaction-items :as api]
            [clj-money.models.transactions :as transactions]))


(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private transaction-items-context
  {:users [(factory :user {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2017 1 1)
                   :description "Paycheck"
                   :quantity 1000M
                   :debit-account-id "Checking"
                   :credit-account-id "Salary"}
                  {:transaction-date (t/local-date 2017 1 1)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 1 8)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 1 15)
                   :description "Paycheck"
                   :quantity 1000M
                   :debit-account-id "Checking"
                   :credit-account-id "Salary"}
                  {:transaction-date (t/local-date 2017 1 15)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 1 22)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 1 29)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 2 1)
                   :description "Paycheck"
                   :quantity 1000M
                   :debit-account-id "Checking"
                   :credit-account-id "Salary"}
                  {:transaction-date (t/local-date 2017 2 5)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 2 12)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 2 15)
                   :description "Paycheck"
                   :quantity 1000M
                   :debit-account-id "Checking"
                   :credit-account-id "Salary"}
                  {:transaction-date (t/local-date 2017 2 19)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 2 26)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}]})

(deftest get-a-list-of-transaction-items-for-an-account
  (let [context (serialization/realize storage-spec transaction-items-context)
        checking (find-account context "Checking")
        user (find-user context "john@doe.com")
        response (with-authentication user
                   (api/index {:criteria {"account-id" (str (:id checking))
                                          "transaction-date" ["between"
                                                              "2018-01-01"
                                                              "2018-01-31"]}
                               :options {"limit" "5"
                                         "sort" "desc"}}))
        actual (:body response)
        expected [{:transaction-date (t/local-date 2017 1 29)
                   :description "Kroger"
                   :quantity 100M
                   :action :credit}
                  {:transaction-date (t/local-date 2017 1 22)
                   :description "Kroger"
                   :quantity 100M
                   :action :credit}
                  {:transaction-date (t/local-date 2017 1 15)
                   :description "Paycheck"
                   :quantity 1000M
                   :action :debit}
                  {:transaction-date (t/local-date 2017 1 15)
                   :description "Kroger"
                   :quantity 100M
                   :action :credit}
                  {:transaction-date (t/local-date 2017 1 8)
                   :description "Kroger"
                   :quantity 100M
                   :action :credit}]]
    (is (= 200 (:status response)) "The response is successful")
    (pprint-diff expected actual)
    (is (= expected actual)
        "The query returns items matching the criteria and consistent with the options")))
