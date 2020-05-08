(ns clj-money.api.reports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-money.test-helpers :refer [reset-db]]
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
