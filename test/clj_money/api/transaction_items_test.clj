(ns clj-money.api.transaction-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-time.core :as t]
            [clj-money.util :refer [path
                                    make-series
                                    map->query-string]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.web.test-helpers :refer [assert-successful]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-entity
                                            find-account
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  (assoc basic-context
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
                         :credit-account-id "Checking"}]
         :reconciliations [{:end-of-period (t/local-date 2017 1 31)
                            :account-id "Checking"
                            :item-refs [{:transaction-date (t/local-date 2017 1 1)
                                         :quantity 1000M}
                                        {:transaction-date (t/local-date 2017 1 8)
                                         :quantity 100M}]
                            :balance 900M
                            :status :completed}
                           {:end-of-period (t/local-date 2017 2 28)
                            :account-id "Checking"
                            :item-refs [{:transaction-date (t/local-date 2017 1 14)
                                         :quantity 100M}]
                            :balance 1000M
                            :status :new}]))

(defn- get-a-list
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        account (find-account ctx "Checking")
        response (-> (req/request :get (str (path :api
                                                  :accounts
                                                  (:id account)
                                                  :transaction-items)
                                            "?"
                                            (map->query-string {:limit 5
                                                                :unreconciled true
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
                   :polarized-quantity -100.0
                   :action "credit"}
                  {:transaction-date "2017-01-22"
                   :description "Kroger"
                   :quantity 100.0
                   :polarized-quantity -100.0
                   :action "credit"}
                  {:transaction-date "2017-01-15"
                   :description "Paycheck"
                   :quantity 1000.0
                   :polarized-quantity 1000.0
                   :action "debit"}
                  {:transaction-date "2017-01-14"
                   :description "Kroger"
                   :quantity 100.0
                   :polarized-quantity -100.0
                   :action "credit"}
                  {:transaction-date "2017-01-01"
                   :description "Kroger"
                   :quantity 100.0
                   :polarized-quantity -100.0
                   :action "credit"}]
        actual (mapv #(select-keys % [:transaction-date
                                      :description
                                      :quantity
                                      :polarized-quantity
                                      :action])
                     body)]
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

(def ^:private children-context
  (-> basic-context
      (update-in [:accounts] concat [{:name "Savings"
                                      :type :asset
                                      :entity-id "Personal"}
                                     {:name "Kitchen remodel"
                                      :type :asset
                                      :entity-id "Personal"
                                      :parent-id "Savings"}
                                     {:name "Car"
                                      :type :asset
                                      :entity-id "Personal"
                                      :parent-id "Savings"}])
      (assoc :transactions [{:transaction-date (t/local-date 2015 1 1)
                             :description "Paycheck"
                             :debit-account-id "Checking"
                             :credit-account-id "Salary"
                             :quantity 2000M}
                            {:transaction-date (t/local-date 2015 1 2)
                             :description "For a rainy day"
                             :debit-account-id "Savings"
                             :credit-account-id "Checking"
                             :quantity 101M}
                            {:transaction-date (t/local-date 2015 1 3)
                             :description "For a Tesla"
                             :debit-account-id "Car"
                             :credit-account-id "Checking"
                             :quantity 102M}
                            {:transaction-date (t/local-date 2015 1 4)
                             :description "For the sub-zero"
                             :debit-account-id "Kitchen remodel"
                             :credit-account-id "Checking"
                             :quantity 103M}])))

(deftest a-user-can-get-a-list-of-items-in-an-account-and-child-accounts
  (let [ctx (realize children-context)
        user (find-user ctx "john@doe.com")
        account (find-account ctx "Savings")
        response (-> (req/request :get (str (path :api
                                                  :accounts
                                                  (:id account)
                                                  :transaction-items)
                                            "?"
                                            (map->query-string {:include-children true})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    (assert-successful response)
    (is (= #{101.0 102.0 103.0}
           (transduce (map :quantity) conj #{} body))
        "The items in the specified account and the children accounts are returned.")))

(def ^:private summary-context
  (assoc basic-context
         :transactions (concat (make-series
                                 {:description "Kroger"
                                  :entity-id "Personal"
                                  :debit-account-id "Groceries"
                                  :credit-account-id "Checking"}
                                 {:quantity 100M
                                  :transaction-date (t/local-date 2016 1 2)}
                                 {:quantity 101M
                                  :transaction-date (t/local-date 2016 1 16)}
                                 {:quantity 102M
                                  :transaction-date (t/local-date 2016 3 1)})
                               (make-series
                                 {:description "Paycheck"
                                  :entity-id "Personal"
                                  :debit-account-id "Checking"
                                  :credit-account-id "Salary"}
                                 {:quantity 1000M
                                  :transaction-date (t/local-date 2016 1 1)}
                                 {:quantity 1000M
                                  :transaction-date (t/local-date 2016 1 15)}
                                 {:quantity 1000M
                                  :transaction-date (t/local-date 2016 2 1)}))))

(defn- summarize-items
  [user-email]
  (let [ctx (realize summary-context)
        entity (find-entity ctx "Personal")
        user (find-user ctx user-email)
        groceries (find-account ctx "Groceries")
        path (format "%s?account-id=%s&transaction-date=2016-01-01&transaction-date=2016-04-30&interval-type=month&interval-count=1"
                     (path :api
                           :entities
                           (:id entity)
                           :transaction-items
                           :summarize)
                     (:id groceries))]
    (-> (req/request :get path)
        (add-auth user)
        app
        parse-json-body)))

(defn- assert-successful-summary
  [{:keys [json-body] :as response}]
  (assert-successful response)
  (is (= [{:start-date "2016-01-01"
           :end-date "2016-01-31"
           :quantity 201.0}
          {:start-date "2016-02-01"
           :end-date "2016-02-29"
           :quantity 0}
          {:start-date "2016-03-01"
           :end-date "2016-03-31"
           :quantity 102.0}
          {:start-date "2016-04-01"
           :end-date "2016-04-30"
           :quantity 0}]
         json-body)))

(defn- assert-blocked-summary
  [{:keys [json-body] :as response}]
  (assert-successful response)
  (is (= [{:start-date "2016-01-01"
           :end-date "2016-01-31"
           :quantity 0}
          {:start-date "2016-02-01"
           :end-date "2016-02-29"
           :quantity 0}
          {:start-date "2016-03-01"
           :end-date "2016-03-31"
           :quantity 0}
          {:start-date "2016-04-01"
           :end-date "2016-04-30"
           :quantity 0}]
         json-body)))

(deftest a-user-can-summarize-items-in-his-entity
  (assert-successful-summary (summarize-items "john@doe.com")))

(deftest a-user-cannot-summarize-items-in-anothers-entity
  (assert-blocked-summary (summarize-items "jane@doe.com")))
