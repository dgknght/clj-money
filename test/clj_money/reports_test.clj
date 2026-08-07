(ns clj-money.reports-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
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
                           (reports/income-statement (entities/find (find-entity "Personal"))
                                                     (t/local-date 2016 1 1)
                                                     (t/local-date 2016 1 31)))
        "The return is a report data structure with income and expense accounts")))

(deftest create-a-balance-sheet-report
  (with-context fixtures/report-context
    (is (seq-of-maps-like? fixtures/expected-balance-sheet
                           (reports/balance-sheet (entities/find (find-entity "Personal"))
                                                  (t/local-date 2016 1 31)))
        "The report include assets, liabilities, and equity totals")))


(deftest balance-sheet-report-with-commodities
  (with-context fixtures/commodities-context
    (is (seq-of-maps-like? fixtures/expected-balance-sheet-with-commodities
                           (reports/balance-sheet (entities/find (find-entity "Personal"))
                                                  (t/local-date 2017 3 2)))
        "The balance sheet includes unrealized gains")))

(deftest create-a-commodities-account-summary
  (with-context fixtures/commodities-account-summary-context
    (is (seq-of-maps-like? (fixtures/expected-commodities-account-summary)
                           (-> "IRA"
                               find-account
                               entities/find
                               (reports/commodities-account-summary (t/local-date 2017 3 2))))
        "The report contains the commodities in the account and their current values based on most recent available price")))

(deftest create-a-budget-report
  (with-context fixtures/budget-context
    (let [report (reports/budget (entities/find-by {:budget/name "2016"}
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
    (let [report (reports/budget (entities/find-by {:budget/name "2016"}
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
    (let [groceries (entities/find-by {:account/name "Groceries"})]
      (testing "half-way through the budget period"
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
                         (reports/monitor groceries
                                          (t/local-date 2016 1 15)))
            "Data reflecting the actual vs prorated budget is returned"))
      (testing "on the first day of the budget period"
        (is (comparable? #:report{:caption "Groceries"
                                  :period #:report{:total-budget 450M
                                                   :prorated-budget 14.516M
                                                   :percentage 1/31
                                                   :actual 0M
                                                   :actual-percent 0M}
                                  :budget #:report{:total-budget 5400M
                                                   :prorated-budget 14.754M
                                                   :percentage 1/366
                                                   :actual 0M
                                                   :actual-percent 0M}}
                         (reports/monitor groceries
                                          (t/local-date 2016 1 1)))
            "Data reflecting the actual vs prorated budget is returned")))))

(def ^:private zero-period-budget-context
  (conj fixtures/base-context
        #:account{:name "Home Maintenance"
                  :entity "Personal"
                  :type :expense}
        #:budget{:name "2016"
                 :entity "Personal"
                 :start-date (t/local-date 2016 01 01)
                 :period [12 :month]
                 :items [#:budget-item{:account "Home Maintenance"
                                       :periods (into [0M] (repeat 11 300M))}]}))

(deftest create-a-budget-monitor-for-a-period-with-no-budgeted-amount
  (with-context zero-period-budget-context
    (let [account (entities/find-by {:account/name "Home Maintenance"})]
      (is (comparable? #:report{:caption "Home Maintenance"
                                :period #:report{:total-budget 0M
                                                 :actual 0M
                                                 :actual-percent 0M}
                                :budget #:report{:total-budget 3300M
                                                 :actual 0M
                                                 :actual-percent 0M}}
                       (reports/monitor account
                                        (t/local-date 2016 1 15)))
          "A zero-dollar period budget does not cause an error and yields a zero actual-percent"))))

(deftest get-a-lot-report
  (with-context fixtures/lot-report-context
    (is (seq-of-maps-like? fixtures/expected-lot-report
                           (reports/lot-report (find-account "IRA")))
        "The report contains lot information grouped by commodity")))

(defn- test-portfolio
  [as-of grouping]
  (let [entity (entities/find (find-entity "Personal"))
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

(deftest portfolio-with-nested-trading-account
  (with-context fixtures/nested-portfolio-context
    (let [entity (entities/find (find-entity "Personal"))
          result (reports/portfolio {:aggregate :by-account
                                     :entity entity
                                     :as-of (t/local-date 2015 4 30)})]
      (is (seq (remove #(= :summary (:report/style %)) result))
          "Trading accounts nested under a parent appear in the portfolio report"))))

(deftest portfolio-with-cash-only-nested-trading-account
  (with-context fixtures/cash-only-nested-portfolio-context
    (let [entity (entities/find (find-entity "Personal"))
          result (reports/portfolio {:aggregate :by-account
                                     :entity entity
                                     :as-of (t/local-date 2015 4 30)})]
      (is (seq (remove #(= :summary (:report/style %)) result))
          "A cash-only trading account nested under a parent appears in the portfolio report"))))

(def ^:private tagged-income-account-context
  (conj fixtures/budget-context
        #:account{:name "Investment Expenses"
                  :entity "Personal"
                  :user-tags #{:investment}
                  :type :expense}
        #:account{:name "Investment Income"
                  :entity "Personal"
                  :type :income}
        #:account{:name "Long Term Gains"
                  :entity "Personal"
                  :user-tags #{:investment}
                  :type :income
                  :parent "Investment Income"}
        #:account{:name "Short Term Gains"
                  :entity "Personal"
                  :user-tags #{:investment}
                  :type :income
                  :parent "Investment Income"}
        #:transaction{:transaction-date (t/local-date 2016 01 10)
                      :entity "Personal"
                      :description "Gain"
                      :items [#:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 500M}
                              #:transaction-item{:action :credit
                                                 :account "Long Term Gains"
                                                 :quantity 500M}]}))

(deftest create-a-budget-report-when-an-income-account-has-a-group-tag
  (with-context tagged-income-account-context
    (let [report (reports/budget (entities/find-by {:budget/name "2016"}
                                                    {:include #{:budget/items}})
                                 {:as-of (t/local-date 2016 2 29)
                                  :tags [:tax :investment :mandatory :discretionary]})
          by-caption (->> (:items report)
                          (map (juxt :report/caption identity))
                          (into {}))]
      (is (comparable? #:report{:budget 0M :actual 500M}
                       (get by-caption "Investment"))
          "Income accounts are grouped by tag alongside expense accounts")
      (is (= "Investment Income/Long Term Gains"
             (-> by-caption (get "Investment") :report/items first :report/caption))
          "A tagged income account still appears even when its parent account is untagged")
      (is (comparable? #:report{:budget 4000M :actual 4010M}
                       (get by-caption "Untagged"))
          "Untagged income accounts are grouped separately from tagged ones"))))
