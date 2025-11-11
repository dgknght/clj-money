(ns clj-money.api.transaction-items-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.util :as util]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [basic-context
                                            with-context
                                            find-entity
                                            find-account
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 1 8)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 1 14)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 1 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date (t/local-date 2017 1 22)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 1 29)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 2 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date (t/local-date 2017 2 5)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 2 12)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 2 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date (t/local-date 2017 2 19)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 2 26)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:reconciliation{:end-of-period (t/local-date 2017 1 31)
                         :account "Checking"
                         :items [[(t/local-date 2017 1 1) 1000M]
                                 [(t/local-date 2017 1 8) 100M]]
                         :balance 900M
                         :status :completed}
        #:reconciliation{:end-of-period (t/local-date 2017 2 28)
                         :account "Checking"
                         :items [[(t/local-date 2017 1 14) 100M]]
                         :balance 1000M
                         :status :new}))

(defn- get-a-list
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [account (find-account "Checking")]
    (-> (request :get (str (path :api
                                 :accounts
                                 (:id account)
                                 :transaction-items)
                           "?"
                           (map->query-string
                             {:limit 5
                              :unreconciled true
                              :transaction-date-on-or-after "2017-01-01"
                              :transaction-date-before "2017-02-01"}))
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body parsed-body]}
   & {:keys [expected]
      :or {expected [{:transaction/transaction-date (t/local-date 2017 01 29)
                      :transaction/description "Kroger"
                      :transaction-item/quantity 100M
                      :transaction-item/polarized-quantity -100M
                      :transaction-item/action :credit}
                     {:transaction/transaction-date (t/local-date 2017 01 22)
                      :transaction/description "Kroger"
                      :transaction-item/quantity 100M
                      :transaction-item/polarized-quantity -100M
                      :transaction-item/action :credit}
                     {:transaction/transaction-date (t/local-date 2017 01 15)
                      :transaction/description "Paycheck"
                      :transaction-item/quantity 1000M
                      :transaction-item/polarized-quantity 1000M
                      :transaction-item/action :debit}
                     {:transaction/transaction-date (t/local-date 2017 01 14)
                      :transaction/description "Kroger"
                      :transaction-item/quantity 100M
                      :transaction-item/polarized-quantity -100M
                      :transaction-item/action :credit}
                     {:transaction/transaction-date (t/local-date 2017 01 01)
                      :transaction/description "Kroger"
                      :transaction-item/quantity 100M
                      :transaction-item/polarized-quantity -100M
                      :transaction-item/action :credit}]}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (seq-of-maps-like? expected body)
        "The transaction items are returned in the response")))

(defn- assert-blocked-list
  [{:as response :keys [edn-body parsed-body]}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (empty? body) "No transaction items are returned")))

(deftest a-user-can-get-a-list-of-transaction-items-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-list (get-a-list "john@doe.com")))
    (testing "json format"
      (assert-successful-list
        (get-a-list "john@doe.com" :content-type "application/json")
        :expected [{:description "Kroger"
                    :quantity {:d 100.0}
                    :action "credit"
                    :_type "transaction-item"}
                   {:description "Kroger"
                    :quantity {:d 100.0}
                    :action "credit"
                    :_type "transaction-item"}
                   {:description "Paycheck"
                    :quantity {:d 1000.0}
                    :action "debit"
                    :_type "transaction-item"}
                   {:description "Kroger"
                    :quantity {:d 100.0}
                    :action "credit"
                    :_type "transaction-item"}
                   {:description "Kroger"
                    :quantity {:d 100.0}
                    :action "credit"
                    :_type "transaction-item"}]))))

(deftest a-user-cannot-get-a-list-of-transaction-items-in-anothers-entity
  (with-context context
    (assert-blocked-list (get-a-list "jane@doe.com"))))

(def ^:private children-context
  (conj basic-context
        #:account{:name "Savings"
                  :type :asset
                  :entity "Personal"}
        #:account{:name "Kitchen reentity"
                  :type :asset
                  :entity "Personal"
                  :parent "Savings"}
        #:account{:name "Car"
                  :type :asset
                  :entity "Personal"
                  :parent "Savings"}
        #:transaction{:transaction-date (t/local-date 2015 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 2000M}
        #:transaction{:transaction-date (t/local-date 2015 1 2)
                      :entity "Personal"
                      :description "For a rainy day"
                      :debit-account "Savings"
                      :credit-account "Checking"
                      :quantity 101M}
        #:transaction{:transaction-date (t/local-date 2015 1 3)
                      :entity "Personal"
                      :description "For a Tesla"
                      :debit-account "Car"
                      :credit-account "Checking"
                      :quantity 102M}
        #:transaction{:transaction-date (t/local-date 2015 1 4)
                      :entity "Personal"
                      :description "For the sub-zero"
                      :debit-account "Kitchen reentity"
                      :credit-account "Checking"
                      :quantity 103M}))

(deftest a-user-can-get-a-list-of-items-in-an-account-and-child-accounts
  (with-context children-context
    (let [account (find-account "Savings")
          response (-> (req/request
                         :get
                         (str (path :api
                                    :accounts
                                    (:id account)
                                    :transaction-items)
                              "?"
                              (map->query-string
                                {:include-children true
                                 :transaction-date-on-or-after "2015-01-01"
                                 :transaction-date-before "2015-02-01"})))
                       (add-auth (find-user "john@doe.com"))
                       app
                       parse-edn-body)]
      (is (http-success? response))
      (is (seq-of-maps-like? [{:transaction-item/quantity 103M
                               :transaction/transaction-date (t/local-date 2015 01 04)
                               :transaction/description "For the sub-zero"}
                              {:transaction-item/quantity 102M
                               :transaction/transaction-date (t/local-date 2015 01 03)
                               :transaction/description "For a Tesla"}
                              {:transaction-item/quantity 101M
                               :transaction/transaction-date (t/local-date 2015 01 02)
                               :transaction/description "For a rainy day"}]
                             (:edn-body response))
          "The items in the specified account and the children accounts are returned."))))

(def ^:private summary-context
  (concat basic-context
          (util/make-series
            #:transaction{:description "Kroger"
                          :entity "Personal"
                          :debit-account "Groceries"
                          :credit-account "Checking"}
            #:transaction{:quantity 100M
                          :transaction-date (t/local-date 2016 1 2)}
            #:transaction{:quantity 101M
                          :transaction-date (t/local-date 2016 1 16)}
            #:transaction{:quantity 102M
                          :transaction-date (t/local-date 2016 3 1)})
          (util/make-series
            #:transaction{:description "Paycheck"
                          :entity "Personal"
                          :debit-account "Checking"
                          :credit-account "Salary"}
            #:transaction{:quantity 1000M
                          :transaction-date (t/local-date 2016 1 1)}
            #:transaction{:quantity 1000M
                          :transaction-date (t/local-date 2016 1 15)}
            #:transaction{:quantity 1000M
                          :transaction-date (t/local-date 2016 2 1)})))

(defn- summarize-items
  [user-email]
  (with-context summary-context
    (let [entity (find-entity "Personal")
          groceries (find-account "Groceries")
          path (format "%s?account-id=%s&transaction-date=2016-01-01&transaction-date=2016-04-30&period-type=month&period-count=1"
                       (path :api
                             :entities
                             (:id entity)
                             :transaction-items
                             :summarize)
                       (:id groceries))]
      (-> (req/request :get path)
          (add-auth (find-user user-email))
          app
          parse-edn-body))))

(defn- assert-successful-summary
  [{:keys [edn-body] :as response}]
  (is (http-success? response))
  (is (= [{:start-date (t/local-date 2016 1 1)
           :end-date (t/local-date 2016 1 31)
           :quantity 201M}
          {:start-date (t/local-date 2016 2 1)
           :end-date (t/local-date 2016 2 29)
           :quantity 0M}
          {:start-date (t/local-date 2016 3 1)
           :end-date (t/local-date 2016 3 31)
           :quantity 102M}
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
