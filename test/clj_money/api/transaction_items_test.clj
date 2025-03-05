(ns clj-money.api.transaction-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.util :refer [make-series]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-entity
                                            find-account
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
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
        account (find-account ctx "Checking")]
    (-> (req/request :get (str (path :api
                                     :accounts
                                     (:id account)
                                     :transaction-items)
                               "?"
                               (map->query-string {:limit 5
                                                   :unreconciled true
                                                   :transaction-date [(t/local-date 2017 1 1) (t/local-date 2017 1 31)]})))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:transaction-date (t/local-date 2017 1 29)
                           :description "Kroger"
                           :quantity 100.0M
                           :polarized-quantity -100.0M
                           :action :credit}
                          {:transaction-date (t/local-date 2017 1 22)
                           :description "Kroger"
                           :quantity 100.0M
                           :polarized-quantity -100.0M
                           :action :credit}
                          {:transaction-date (t/local-date 2017 1 15)
                           :description "Paycheck"
                           :quantity 1000.0M
                           :polarized-quantity 1000.0M
                           :action :debit}
                          {:transaction-date (t/local-date 2017 1 14)
                           :description "Kroger"
                           :quantity 100.0M
                           :polarized-quantity -100.0M
                           :action :credit}
                          {:transaction-date (t/local-date 2017 1 1)
                           :description "Kroger"
                           :quantity 100.0M
                           :polarized-quantity -100.0M
                           :action :credit}]
                         edn-body)
      "The correct transaction items are returned in the response"))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "No transaction items are returned"))

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
                     app
                     parse-edn-body)]
    (is (http-success? response))
    (is (= #{101.0M 102.0M 103.0M}
           (transduce (map :quantity) conj #{} (:edn-body response)))
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
        parse-edn-body)))

(defn- assert-successful-summary
  [{:keys [edn-body] :as response}]
  (is (http-success? response))
  (is (= [{:start-date (t/local-date 2016 1 1)
           :end-date (t/local-date 2016 1 31)
           :quantity 201.0M}
          {:start-date (t/local-date 2016 2 1)
           :end-date (t/local-date 2016 2 29)
           :quantity 0M}
          {:start-date (t/local-date 2016 3 1)
           :end-date (t/local-date 2016 3 31)
           :quantity 102.0M}
          {:start-date (t/local-date 2016 4 1)
           :end-date (t/local-date 2016 4 30)
           :quantity 0M}]
         edn-body)))

(defn- assert-blocked-summary
  [{:keys [edn-body] :as response}]
  (is (http-success? response))
  (is (= [{:start-date (t/local-date 2016 1 1)
           :end-date (t/local-date 2016 1 31)
           :quantity 0M}
          {:start-date (t/local-date 2016 2 1)
           :end-date (t/local-date 2016 2 29)
           :quantity 0M}
          {:start-date (t/local-date 2016 3 1)
           :end-date (t/local-date 2016 3 31)
           :quantity 0M}
          {:start-date (t/local-date 2016 4 1)
           :end-date (t/local-date 2016 4 30)
           :quantity 0M}]
         edn-body)))

(deftest a-user-can-summarize-items-in-his-entity
  (assert-successful-summary (summarize-items "john@doe.com")))

(deftest a-user-cannot-summarize-items-in-anothers-entity
  (assert-blocked-summary (summarize-items "jane@doe.com")))
