(ns clj-money.api.reports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-account
                                            find-user
                                            find-entity
                                            find-budget]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private report-context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2015 1 1)
                      :description "Paycheck"
                      :entity "Personal"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}))

(defn- get-income-statement
  [email]
  (with-context report-context
    (let [entity (find-entity "Personal")]
      (-> (req/request :get (path :api
                                  :entities
                                  (:id entity)
                                  :reports
                                  :income-statement
                                  "2016-01-01"
                                  "2016-01-31"))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(defn- assert-successful-income-statement
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= ["Income" "Expense" "Net"]
         (->> edn-body
              (filter #(#{:header :summary}
                         (:report/style %)))
              (map :report/caption)))
      "The body contains the income statement report for the specified entity"))

(defn- assert-blocked-income-statement
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-an-income-statement-for-his-entity
  (assert-successful-income-statement (get-income-statement "john@doe.com")))

(deftest a-user-cannot-get-an-income-statement-for-anothers-entity
  (assert-blocked-income-statement (get-income-statement "jane@doe.com")))

(defn- get-balance-sheet
  [email]
  (with-context report-context
    (let [entity (find-entity "Personal")]
      (with-fixed-time "2016-02-02T00:00:00Z"
        (-> (req/request :get (path :api
                                    :entities
                                    (:id entity)
                                    :reports
                                    :balance-sheet
                                    "2016-01-31"))
            (add-auth (find-user email))
            app
            parse-edn-body)))))

(defn- assert-successful-balance-sheet
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= ["Asset" "Liability" "Equity" "Liabilities + Equity"]
         (->> edn-body
              (filter #(#{:summary :header} (:report/style %)))
              (map :report/caption)))
      "The body contains the balance sheet report"))

(defn- assert-blocked-balance-sheet
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-an-balance-sheet-for-his-entity
  (assert-successful-balance-sheet (get-balance-sheet "john@doe.com")))

(deftest a-user-cannot-get-an-balance-sheet-for-anothers-entity
  (assert-blocked-balance-sheet (get-balance-sheet "jane@doe.com")))

(def budget-context
  (conj report-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :start-date (t/local-date 2016 1 1)
                 :period [12 :month]}))

(defn- get-budget-report
  [email]
  (with-context budget-context
    (let [budget (find-budget "2016")]
      (-> (req/request :get (path :api
                                  :reports
                                  :budget
                                  (:id budget)))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(defn- assert-successful-budget-report
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= "2016: January to December"
         (:title edn-body))
      "The response contains the report tital")
  (is (= ["Income" "Expense" "Net"]
         (->> (:items edn-body)
              (filter #(#{:header :summary} (:report/style %)))
              (map :report/caption)))
      "The reponse contains the budget report at the :items key"))

(defn- assert-blocked-budget-report
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-budget-report-for-his-entity
  (assert-successful-budget-report (get-budget-report "john@doe.com")))

(deftest a-user-cannot-get-a-budget-report-for-anothers-entity
  (assert-blocked-budget-report (get-budget-report "jane@doe.com")))

(def ^:private monitor-context
  (conj report-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :start-date (t/local-date 2016 1 1)
                 :period [12 :month]
                 :items [#:budget-item{:account "Groceries"
                                       :periods (repeat 12 200M)}]}
        #:transaction{:transaction-date (t/local-date 2016 1 1)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 85M
                      :debit-account "Groceries"
                      :credit-account "Checking"}))

(defn- get-monitor-list
  [email]
  (with-context monitor-context
    (let [entity (find-entity "Personal")]

      ; TODO: I'd like to move this into the context, but I've got to
      ; solve the chicken and egg problem first
      (models/put (update-in entity
                             [:entity/settings
                              :settings/monitored-accounts]
                             (fnil conj #{})
                             (util/->model-ref (find-account "Groceries"))))

      (with-fixed-time "2016-01-07T00:00:00Z"
        (-> (req/request :get (path :api
                                    :entities
                                    (:id entity)
                                    :reports
                                    :budget-monitors))
            (add-auth (find-user email))
            app
            parse-edn-body)))))

(defn- assert-successful-monitor-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:report{:caption "Groceries"
                                   :period #:report{:total-budget 200M
                                                    :actual 85M
                                                    :percentage 0.2258M
                                                    :prorated-budget 45.162M
                                                    :actual-percent 0.425M}
                                   :budget #:report{:total-budget 2400M
                                                    :actual 85M
                                                    :percentage 0.0191M
                                                    :prorated-budget 45.902M
                                                    :actual-percent 0.035417M}}]
                         edn-body)))

(defn- assert-blocked-monitor-list
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-budget-monitors-for-his-entity
  (assert-successful-monitor-list (get-monitor-list "john@doe.com")))

(deftest a-user-cannot-get-budget-monitors-for-anothers-entity
  (assert-blocked-monitor-list (get-monitor-list "jane@doe.com")))

(def ^:private portfolio-context
  (conj basic-context
        #:account{:name "IRA"
                  :type :asset
                  :system-tags #{:trading}
                  :entity "Personal"}
        #:commodity{:name "Apple, Inc."
                    :entity "Personal"
                    :symbol "AAPL"
                    :type :stock
                    :exchange :nasdaq}
        #:transaction{:transaction-date (t/local-date 2015 1 1)
                      :entity "Personal"
                      :description "Begining balance"
                      :quantity 1000M
                      :debit-account "IRA"
                      :credit-account "Opening Balances"}
        #:trade{:date (t/local-date 2015 2 1)
                :entity "Personal"
                :type :purchase
                :account "IRA"
                :commodity "AAPL"
                :shares 100M
                :value  1000M}))

(defn- get-portfolio-report
  [email]
  (with-context portfolio-context
    (let [entity (find-entity "Personal")]
      (-> (req/request :get (str (path :api
                                       :entities
                                       (:id entity)
                                       :reports
                                       :portfolio)
                                 "?aggregate=by-account"))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(defn- assert-successful-portfolio-report
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= ["IRA" "Total"]
         (->> edn-body
              (filter #(#{:header :summary} (:report/style %)))
              (map :report/caption)))
      "The body contains the correct captions"))

(defn- assert-blocked-portfolio-report
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-portfolio-report-for-his-entity
  (assert-successful-portfolio-report (get-portfolio-report "john@doe.com")))

(deftest a-user-cannot-get-a-portfolio-report-for-anothers-entity
 (assert-blocked-portfolio-report (get-portfolio-report "jane@doe.com")))
