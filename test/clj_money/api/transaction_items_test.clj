(ns clj-money.api.transaction-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.x-platform.util :refer [path
                                               map->query-string]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.test-helpers :refer [assert-successful]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-account
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff]]
            [clj-money.web.server :refer [app]]))


(use-fixtures :each (partial reset-db (env :db)))

(def ^:private context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
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
                  {:transaction-date (t/local-date 2017 1 14)
                   :description "Kroger"
                   :quantity 100M
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"}
                  {:transaction-date (t/local-date 2017 1 15)
                   :description "Paycheck"
                   :quantity 1000M
                   :debit-account-id "Checking"
                   :credit-account-id "Salary"}
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

(defn- get-a-list
  [email]
  (let [ctx (realize (env :db) context)
        user (find-user ctx email)
        account (find-account ctx "Checking")
        response (-> (req/request :get (str (path :api
                                                  :accounts
                                                  (:id account)
                                                  :transaction-items)
                                            "?"
                                            (map->query-string {:limit 5
                                                                :start-date "2017-01-01"
                                                                :end-date "2017-01-31"})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (assert-successful response)
  (let [expected [{:transaction-date "2017-01-29"
                   :description "Kroger"
                   :quantity 100.0
                   :action "credit"}
                  {:transaction-date "2017-01-22"
                   :description "Kroger"
                   :quantity 100.0
                   :action "credit"}
                  {:transaction-date "2017-01-15"
                   :description "Paycheck"
                   :quantity 1000.0
                   :action "debit"}
                  {:transaction-date "2017-01-14"
                   :description "Kroger"
                   :quantity 100.0
                   :action "credit"}
                  {:transaction-date "2017-01-08"
                   :description "Kroger"
                   :quantity 100.0
                   :action "credit"}]
        actual (mapv #(select-keys % [:transaction-date :description :quantity :action])
                     body)]
    (pprint-diff expected actual)
    (is (= expected actual)
        "The correct transaction items are returned in the response")))

(defn- assert-blocked-list
  [[response body]]
  (assert-successful response)
  (is (empty? body) "No transaction items are returned"))

(deftest a-user-can-get-a-list-of-transaction-items-in-his-entity
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-transaction-items-in-anothers-entity
  (assert-blocked-list (get-a-list "jane@doe.com")))
