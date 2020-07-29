(ns clj-money.api.reports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff]]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-not-found]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.x-platform.util :refer [path]]
            [clj-money.test-context :refer [realize
                                            basic-context
                                            find-user
                                            find-entity
                                            find-budget]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each (partial reset-db (env :db)))

(def ^:private report-context
  basic-context)

(defn- get-income-statement
  [email]
  (let [ctx (realize (env :db) report-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :get (path :api
                                           :entities
                                           (:id entity)
                                           :reports
                                           :income-statement
                                           "2016-01-01"
                                           "2016-01-31"))
                   (add-auth user)
                   app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-income-statement
  [[response body]]
  (assert-successful response)
  (is (= ["Income" "Expense" "Net"]
         (map :caption body))
      "The body contains the correct captions"))

(defn- assert-blocked-income-statement
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-an-income-statement-for-his-entity
  (assert-successful-income-statement (get-income-statement "john@doe.com")))

(deftest a-user-cannot-get-an-income-statement-for-anothers-entity
  (assert-blocked-income-statement (get-income-statement "jane@doe.com")))

(defn- get-balance-sheet
  [email]
  (let [ctx (realize (env :db) report-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :get (path :api
                                           :entities
                                           (:id entity)
                                           :reports
                                           :balance-sheet
                                           "2016-01-31"))
                   (add-auth user)
                   app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-balance-sheet
  [[response body]]
  (assert-successful response)
  (is (= ["Asset" "Liability" "Equity" "Liabilities + Equity"]
         (map :caption body))
      "The body contains the correct captions"))

(defn- assert-blocked-balance-sheet
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-an-balance-sheet-for-his-entity
  (assert-successful-balance-sheet (get-balance-sheet "john@doe.com")))

(deftest a-user-cannot-get-an-balance-sheet-for-anothers-entity
  (assert-blocked-balance-sheet (get-balance-sheet "jane@doe.com")))

(def budget-context
  (assoc report-context :budgets [{:name "2016"
                                   :entity-id "Personal"
                                   :start-date (t/local-date 2016 1 1)
                                   :period :month
                                   :period-count 12}]))

(defn- get-budget-report
  [email]
  (let [ctx (realize (env :db) budget-context)
        user (find-user ctx email)
        budget (find-budget ctx "2016")
        response (-> (req/request :get (path :api
                                             :reports
                                             :budget
                                             (:id budget)))
                   (add-auth user)
                   app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-budget-report
  [[response body]]
  (assert-successful response)
  (is (coll? body)))

(defn- assert-blocked-budget-report
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-an-budget-report-for-his-entity
  (assert-successful-budget-report (get-budget-report "john@doe.com")))

(deftest a-user-cannot-get-an-budget-report-for-anothers-entity
  (assert-blocked-budget-report (get-budget-report "jane@doe.com")))

(def ^:private monitor-context
  (-> budget-context
      (update-in [:budgets 0] assoc :items [{:account-id "Groceries"
                                             :periods (repeat 12 200)}])
      (update-in [:entities 0] assoc-in [:settings :monitored-account-ids] #{"Groceries"})
      (assoc :transactions [{:transaction-date (t/local-date 2016 1 1)
                             :description "Kroger"
                             :quantity 85M
                             :debit-account-id "Groceries"
                             :credit-account-id "Checking"}])))

(defn- get-monitor-list
  [email]
  (let [ctx (realize (env :db) monitor-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (t/do-at (t/date-time 2016 1 7)
                          (-> (req/request :get (path :api
                                                      :entities
                                                      (:id entity)
                                                      :reports
                                                      :budget-monitors))
                              (add-auth user)
                              app))
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-monitor-list
  [[response body]]
  (assert-successful response)
  (let [expected [{:caption "Groceries"
                   :period {:total-budget 200.0
                            :actual 85.0
                            :percentage 0.2258
                            :prorated-budget 45.162
                            :actual-percent 0.425}
                   :budget {:total-budget 2400.0
                            :actual 85.0
                            :percentage 0.0191
                            :prorated-budget 45.902
                            :actual-percent 0.035417}}]
        actual (when (sequential? body)
                 (map #(dissoc % :account-id) body))]
    (pprint-diff expected actual)
    (is (= expected actual))))

(defn- assert-blocked-monitor-list
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-budget-monitors-for-his-entity
  (assert-successful-monitor-list (get-monitor-list "john@doe.com")))

(deftest a-user-cannot-get-budget-monitors-for-anothers-entity
  (assert-blocked-monitor-list (get-monitor-list "jane@doe.com")))
