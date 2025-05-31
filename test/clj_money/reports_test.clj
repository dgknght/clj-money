(ns clj-money.reports-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.sql.ref]
            [clj-money.models.ref]
            [clj-money.models :as models]
            [clj-money.core]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account]]
            [clj-money.reports :as reports]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(deftest create-an-income-statement
  (with-context fixtures/report-context
    (is (seq-of-maps-like? fixtures/expected-income-statement
                           (reports/income-statement (models/find (find-entity "Personal"))
                                                     (t/local-date 2016 1 1)
                                                     (t/local-date 2016 1 31)))
        "The return is a report data structure with income and expense accounts")))

(deftest create-a-balance-sheet-report
  (with-context fixtures/report-context
    (is (seq-of-maps-like? fixtures/expected-balance-sheet
                           (reports/balance-sheet (models/find (find-entity "Personal"))
                                                  (t/local-date 2016 1 31)))
        "The report include assets, liabilities, and equity totals")))


(deftest balance-sheet-report-with-commodities
  (with-context fixtures/commodities-context
    (is (seq-of-maps-like? fixtures/expected-balance-sheet-with-commodities
                           (reports/balance-sheet (models/find (find-entity "Personal"))
                                                  (t/local-date 2017 3 2)))
        "THe balance sheet includes unrealized gains")))

(deftest create-a-commodities-account-summary
  (with-context fixtures/commodities-account-summary-context
    (is (seq-of-maps-like? (fixtures/expected-commodities-account-summary)
                           (-> "IRA"
                               find-account
                               models/find
                               (reports/commodities-account-summary (t/local-date 2017 3 2))))
        "The report contains the commodities in the account and their current values based on most recent available price")))

(deftest create-a-budget-report
  (with-context fixtures/budget-context
    (let [report (reports/budget (models/find-by {:budget/name "2016"}
                                                 {:include #{:budget/items}})
                                 {:as-of (t/local-date 2016 2 29)})]
      (is (= (:title fixtures/expected-budget-report)
             (:title report))
          "The report title includes the budget name of the periods covered")
      (is (seq-of-maps-like?
            (:items fixtures/expected-budget-report)
            (:items report))
          "The result contains records describing planned income and expenses vs actual"))))

(deftest create-a-budget-report-grouped-by-tags
  (with-context fixtures/budget-context
    (let [report (reports/budget (models/find-by {:budget/name "2016"}
                                                 {:include #{:budget/items}})
                                 {:as-of (t/local-date 2016 2 29)
                                  :tags [:tax :mandatory :discretionary]})]
      (is (= (:title fixtures/expected-budget-report-by-tag)
             (:title report))
          "The report title includes the budget name of the periods covered")
      (is (seq-of-maps-like?
            (:items fixtures/expected-budget-report-by-tag)
            (:items report))
          "The result contains records describing planned income and expenses vs actual, grouped by account tags"))))

(deftest create-a-budget-monitor
  (with-context fixtures/budget-context
    (let [groceries (models/find-by {:account/name "Groceries"})
          ; half-way through january
          report (reports/monitor groceries
                                  (t/local-date 2016 1 15))]
      (is (comparable? #:report{:caption "Groceries"
                                :period #:report{:total-budget 450M
                                                 :prorated-budget 217.74M
                                                 :percentage 15/31
                                                 :actual 200M
                                                 :actual-percent 0.44444M}
                                :budget #:report{:total-budget 5400M
                                                 :prorated-budget 221.31M
                                                 :percentage 15/366
                                                 :actual 200M
                                                 :actual-percent 0.037037M}}
                       report)
          "Data reflecting the actual vs prorated budget is returned"))))

(deftest get-a-lot-report
  (with-context fixtures/lot-report-context
    (is (seq-of-maps-like? fixtures/expected-lot-report
                           (reports/lot-report (find-account "IRA")))
        "The report contains lot information grouped by commodity")))

(defn- test-portfolio
  [as-of grouping]
  (let [entity (models/find (find-entity "Personal"))
        expected (get-in fixtures/expected-portfolio-report
                         [grouping as-of])]
    (is (seq-of-maps-like? expected
                           (reports/portfolio {:aggregate grouping
                                               :entity entity
                                               :as-of as-of}))
        "The data reflects the commodities owned and their values of the specified date")))

(deftest get-a-portfolio-report
  (with-context fixtures/portfolio-context
    (testing "most recent by account"
      (test-portfolio (t/local-date 2015 4 30) :by-account))
    (testing "1 month ago by account"
      (test-portfolio (t/local-date 2015 3 31) :by-account))
    (testing "most recent by commodity"
      (test-portfolio (t/local-date 2015 4 30) :by-commodity))
    (testing "1 month ago by commodity"
      (test-portfolio (t/local-date 2015 3 31) :by-commodity))))
