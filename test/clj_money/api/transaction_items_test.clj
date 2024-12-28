(ns clj-money.api.transaction-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [basic-context
                                            with-context
                                            find-entity
                                            find-account
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db]]
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
                         :item-refs [[(t/local-date 2017 1 1) 1000M]
                                     [(t/local-date 2017 1 8) 100M]]
                         :balance 900M
                         :status :completed}
        #:reconciliation{:end-of-period (t/local-date 2017 2 28)
                         :account "Checking"
                         :item-refs [[(t/local-date 2017 1 14) 100M]]
                         :balance 1000M
                         :status :new}))

(defn- get-a-list
  [email]
  (with-context context
    (let [account (find-account "Checking")]
      (-> (req/request :get (str (path :api
                                       :accounts
                                       (:id account)
                                       :transaction-items)
                                 "?"
                                 (map->query-string {:limit 5
                                                     :unreconciled true
                                                     :transaction-date ["2017-01-01" "2017-02-01"]})))
          (add-auth (find-user email))
          app
          parse-json-body))))

(defn- assert-successful-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:transaction-item/transaction-date "2017-01-29"
                           :transaction/description "Kroger"
                           :transaction-item/quantity 100.0
                           :transaction-item/polarized-quantity -100.0
                           :transaction-item/action "credit"}
                          {:transaction-item/transaction-date "2017-01-22"
                           :transaction/description "Kroger"
                           :transaction-item/quantity 100.0
                           :transaction-item/polarized-quantity -100.0
                           :transaction-item/action "credit"}
                          {:transaction-item/transaction-date "2017-01-15"
                           :transaction/description "Paycheck"
                           :transaction-item/quantity 1000.0
                           :transaction-item/polarized-quantity 1000.0
                           :transaction-item/action "debit"}
                          {:transaction-item/transaction-date "2017-01-14"
                           :transaction/description "Kroger"
                           :transaction-item/quantity 100.0
                           :transaction-item/polarized-quantity -100.0
                           :transaction-item/action "credit"}
                          {:transaction-item/transaction-date "2017-01-01"
                           :transaction/description "Kroger"
                           :transaction-item/quantity 100.0
                           :transaction-item/polarized-quantity -100.0
                           :transaction-item/action "credit"}]
                         json-body)
      "The correct transaction items are returned in the response"))

(defn- assert-blocked-list
  [response]
  (is (http-success? response))
  (is (empty? (:json-body response)) "No transaction items are returned"))

(deftest a-user-can-get-a-list-of-transaction-items-in-his-entity
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-transaction-items-in-anothers-entity
  (assert-blocked-list (get-a-list "jane@doe.com")))

; (def ^:private children-context
;   (-> basic-context
;       (update-in [:accounts] concat [{:name "Savings"
;                                       :type :asset
;                                       :entity-id "Personal"}
;                                      {:name "Kitchen remodel"
;                                       :type :asset
;                                       :entity-id "Personal"
;                                       :parent-id "Savings"}
;                                      {:name "Car"
;                                       :type :asset
;                                       :entity-id "Personal"
;                                       :parent-id "Savings"}])
;       (assoc :transactions [{:transaction-date (t/local-date 2015 1 1)
;                              :description "Paycheck"
;                              :debit-account-id "Checking"
;                              :credit-account-id "Salary"
;                              :quantity 2000M}
;                             {:transaction-date (t/local-date 2015 1 2)
;                              :description "For a rainy day"
;                              :debit-account-id "Savings"
;                              :credit-account-id "Checking"
;                              :quantity 101M}
;                             {:transaction-date (t/local-date 2015 1 3)
;                              :description "For a Tesla"
;                              :debit-account-id "Car"
;                              :credit-account-id "Checking"
;                              :quantity 102M}
;                             {:transaction-date (t/local-date 2015 1 4)
;                              :description "For the sub-zero"
;                              :debit-account-id "Kitchen remodel"
;                              :credit-account-id "Checking"
;                              :quantity 103M}])))
; 
; (deftest a-user-can-get-a-list-of-items-in-an-account-and-child-accounts
;   (let [ctx (realize children-context)
;         user (find-user ctx "john@doe.com")
;         account (find-account ctx "Savings")
;         response (-> (req/request :get (str (path :api
;                                                   :accounts
;                                                   (:id account)
;                                                   :transaction-items)
;                                             "?"
;                                             (map->query-string {:include-children true})))
;                      (add-auth user)
;                      app)
;         body (json/parse-string (:body response) true)]
;     (is (http-success? response))
;     (is (= #{101.0 102.0 103.0}
;            (transduce (map :quantity) conj #{} body))
;         "The items in the specified account and the children accounts are returned.")))
; 
; (def ^:private summary-context
;   (assoc basic-context
;          :transactions (concat (make-series
;                                  {:description "Kroger"
;                                   :entity-id "Personal"
;                                   :debit-account-id "Groceries"
;                                   :credit-account-id "Checking"}
;                                  {:quantity 100M
;                                   :transaction-date (t/local-date 2016 1 2)}
;                                  {:quantity 101M
;                                   :transaction-date (t/local-date 2016 1 16)}
;                                  {:quantity 102M
;                                   :transaction-date (t/local-date 2016 3 1)})
;                                (make-series
;                                  {:description "Paycheck"
;                                   :entity-id "Personal"
;                                   :debit-account-id "Checking"
;                                   :credit-account-id "Salary"}
;                                  {:quantity 1000M
;                                   :transaction-date (t/local-date 2016 1 1)}
;                                  {:quantity 1000M
;                                   :transaction-date (t/local-date 2016 1 15)}
;                                  {:quantity 1000M
;                                   :transaction-date (t/local-date 2016 2 1)}))))
; 
; (defn- summarize-items
;   [user-email]
;   (let [ctx (realize summary-context)
;         entity (find-entity ctx "Personal")
;         user (find-user ctx user-email)
;         groceries (find-account ctx "Groceries")
;         path (format "%s?account-id=%s&transaction-date=2016-01-01&transaction-date=2016-04-30&interval-type=month&interval-count=1"
;                      (path :api
;                            :entities
;                            (:id entity)
;                            :transaction-items
;                            :summarize)
;                      (:id groceries))]
;     (-> (req/request :get path)
;         (add-auth user)
;         app
;         parse-json-body)))
; 
; (defn- assert-successful-summary
;   [{:keys [json-body] :as response}]
;   (is (http-success? response))
;   (is (= [{:start-date "2016-01-01"
;            :end-date "2016-01-31"
;            :quantity 201.0}
;           {:start-date "2016-02-01"
;            :end-date "2016-02-29"
;            :quantity 0}
;           {:start-date "2016-03-01"
;            :end-date "2016-03-31"
;            :quantity 102.0}
;           {:start-date "2016-04-01"
;            :end-date "2016-04-30"
;            :quantity 0}]
;          json-body)))
; 
; (defn- assert-blocked-summary
;   [{:keys [json-body] :as response}]
;   (is (http-success? response))
;   (is (= [{:start-date "2016-01-01"
;            :end-date "2016-01-31"
;            :quantity 0}
;           {:start-date "2016-02-01"
;            :end-date "2016-02-29"
;            :quantity 0}
;           {:start-date "2016-03-01"
;            :end-date "2016-03-31"
;            :quantity 0}
;           {:start-date "2016-04-01"
;            :end-date "2016-04-30"
;            :quantity 0}]
;          json-body)))
; 
; (deftest a-user-can-summarize-items-in-his-entity
;   (assert-successful-summary (summarize-items "john@doe.com")))
; 
; (deftest a-user-cannot-summarize-items-in-anothers-entity
;   (assert-blocked-summary (summarize-items "jane@doe.com")))
