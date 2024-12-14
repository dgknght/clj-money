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
                                            find-budget
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
    (let [report (reports/budget (find-budget "2016")
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
    (let [report (reports/budget (find-budget "2016")
                                 {:as-of (t/local-date 2016 2 29)
                                  :tags [:tax :mandatory :discretionary]})]
      (is (= (:title fixtures/expected-budget-report-by-tag)
             (:title report))
          "The report title includes the budget name of the periods covered")
      (is (seq-of-maps-like?
            (:items fixtures/expected-budget-report-by-tag)
            (:items report))
          "The result contains records describing planned income and expenses vs actual, grouped by account tags"))))

; (deftest create-a-budget-monitor
;   (with-context budget-report-context
;     (let [groceries (accounts/find-by {:name "Groceries"})
; 
;           ; half-way through january
;           actual (-> (reports/monitor groceries
;                                       (t/local-date 2016 1 15))
;                      (dissoc :account))
;           expected {:caption "Groceries"
;                     :period {:total-budget 450M
;                              :prorated-budget 217.74M
;                              :percentage 15/31
;                              :actual 200M
;                              :actual-percent 0.44444M}
;                     :budget {:total-budget 5400M
;                              :prorated-budget 221.31M
;                              :percentage 15/366
;                              :actual 200M
;                              :actual-percent 0.037037M}}]
;       (is (= expected actual) "The correct information is returned"))))
; 
; (deftest get-a-lot-report
;   (let [context (realize commodities-context)
;         [ira
;          lt-gains
;          st-gains
;          lt-losses
;          st-losses] (find-accounts context "IRA"
;                                    "LT Gains"
;                                    "ST Gains"
;                                    "LT Losses"
;                                    "ST Losses")
;         [aapl
;          msft
;          ge] (find-commodities context "AAPL"
;                                "MSFT"
;                                "GE")
;         p1 (trading/buy {:trade-date (t/local-date 2017 1 15)
;                          :commodity (:id aapl)
;                          :account (:id ira)
;                          :shares 10M
;                          :value 100M})
;         p2 (trading/buy {:trade-date (t/local-date 2017 1 15)
;                          :commodity (:id msft)
;                          :account (:id ira)
;                          :shares 10M
;                          :value 100M})
;         p3 (trading/buy {:trade-date (t/local-date 2017 1 15)
;                          :commodity (:id ge)
;                          :account (:id ira)
;                          :shares 10M
;                          :value 100M})
;         s1 (trading/sell {:trade-date (t/local-date 2017 1 31)
;                           :commodity (:id aapl)
;                           :account (:id ira)
;                           :shares 5M
;                           :value 55M
;                           :lt-capital-gains-account (:id lt-gains)
;                           :st-capital-gains-account (:id st-gains)
;                           :lt-capital-loss-account (:id lt-losses)
;                           :st-capital-loss-account (:id st-losses)})
;         actual (map (fn [record]
;                       (update-in record [:transactions] (fn [transactions]
;                                                           (map #(dissoc % :lot)
;                                                                transactions))))
;                     (reports/lot-report (:id ira)))
;         expected [{:caption "Apple, Inc. (AAPL)"
;                    :commodity (:id aapl)
;                    :purchase-date (t/local-date 2017 1 15)
;                    :shares-owned 5M
;                    :purchase-price 10M
;                    :cost 50M
;                    :current-price 20M
;                    :value 100M
;                    :gain 50M
;                    :transactions [{:transaction-date (t/local-date 2017 1 15)
;                                    :id (-> p1 :transaction :id)
;                                    :lot-action :buy
;                                    :price 10M
;                                    :shares 10M}
;                                   {:transaction-date (t/local-date 2017 1 31)
;                                    :id (-> s1 :transaction :id)
;                                    :lot-action :sell
;                                    :price 11M
;                                    :shares 5M}]}
;                   {:caption "General Electric Co. (GE)"
;                    :commodity (:id ge)
;                    :purchase-date (t/local-date 2017 1 15)
;                    :shares-owned 10M
;                    :purchase-price 10M
;                    :cost 100M
;                    :value 100M
;                    :current-price 10M
;                    :gain 0M
;                    :transactions [{:transaction-date (t/local-date 2017 1 15)
;                                    :id (-> p3 :transaction :id)
;                                    :lot-action :buy
;                                    :shares 10M
;                                    :price 10M}]}
;                   {:caption "Microsoft Corp (MSFT)"
;                    :commodity (:id msft)
;                    :purchase-date (t/local-date 2017 1 15)
;                    :shares-owned 10M
;                    :purchase-price 10M
;                    :cost 100M
;                    :value 50M
;                    :current-price 5M
;                    :gain -50M
;                    :transactions [{:transaction-date (t/local-date 2017 1 15)
;                                    :id (-> p2 :transaction :id)
;                                    :lot-action :buy
;                                    :shares 10M
;                                    :price 10M}]}]]
;     (when (not= expected actual)
;       (pprint {:expected expected
;                :actual actual
;                :diff (diff expected actual)}))
;     (is (= expected actual)
;         "The report contains the correct data")))
; 
; (def ^:private portfolio-context
;   (-> basic-context
;       (update-in [:accounts] concat [{:name "IRA"
;                                       :type :asset
;                                       :tags #{:trading}
;                                       :entity "Personal"}
;                                      {:name "401k"
;                                       :type :asset
;                                       :tags #{:trading}
;                                       :entity "Personal"}])
;       (update-in [:commodities] concat [{:name "Apple, Inc."
;                                          :symbol "AAPL"
;                                          :type :stock
;                                          :exchange :nasdaq}
;                                         {:name "Microsoft, Inc."
;                                          :symbol "MSFT"
;                                          :type :stock
;                                          :exchange :nasdaq}
;                                         {:name "Alphabet, Inc."
;                                          :symbol "GOOG"
;                                          :type :stock
;                                          :exchange :nasdaq}])
;       (assoc :transactions [{:transaction-date (t/local-date 2015 1 1)
;                              :description "Begining balance"
;                              :quantity 10000M
;                              :debit-account "401k"
;                              :credit-account "Opening Balances"}
;                             {:transaction-date (t/local-date 2015 1 1)
;                              :description "Begining balance"
;                              :quantity 10000M
;                              :debit-account "IRA"
;                              :credit-account "Opening Balances"}]
;              :trades [{:trade-date (t/local-date 2015 2 1)
;                        :type :purchase
;                        :account "IRA"
;                        :commodity "AAPL"
;                        :shares 200M
;                        :value 2000M} ; $10.00/share
;                       {:trade-date (t/local-date 2015 3 1)
;                        :type :purchase
;                        :account "IRA"
;                        :commodity "AAPL"
;                        :shares 100M
;                        :value 1100M} ; $11.00/share
;                       {:trade-date (t/local-date 2015 4 1)
;                        :type :sale
;                        :account "IRA"
;                        :commodity "AAPL"
;                        :shares 100M
;                        :value 1200M} ; $12.00/share
;                       {:trade-date (t/local-date 2015 2 1)
;                        :type :purchase
;                        :account "401k"
;                        :commodity "MSFT"
;                        :shares 400M
;                        :value 2000M} ; $4.00/share
;                       {:trade-date (t/local-date 2015 3 1)
;                        :type :purchase
;                        :account "401k"
;                        :commodity "MSFT"
;                        :shares 200M
;                        :value 800M} ; $4.00/share
;                       {:trade-date (t/local-date 2015 4 1)
;                        :type :purchase
;                        :account "401k"
;                        :commodity "MSFT"
;                        :shares 100M
;                        :value 300M}]))) ; $3.00/share
; 
; (def portfolio-fixture
;   (read-string (slurp "resources/fixtures/reports_test/portfolio.edn")))
; 
; (defn- test-portfolio
;   [ctx as-of grouping]
;   (let [entity (find-entity ctx "Personal")
;         expected (get-in portfolio-fixture [:expected grouping as-of])
;         actual (map #(select-keys % [:caption
;                                      :style
;                                      :shares-purchased
;                                      :shares-owned
;                                      :cost-basis
;                                      :current-value
;                                      :gain-loss
;                                      :gain-loss-percent])
;                     (reports/portfolio {:aggregate grouping
;                                         :entity entity
;                                         :as-of as-of}))]
;     (is (= expected actual)
;         "The correct report data is generated")))
; 
; (deftest get-a-portfolio-report-by-account
;   (let [ctx (realize portfolio-context)]
;     (testing "most recent"
;       (test-portfolio ctx (t/local-date 2015 4 30) :by-account))
;     (testing "1 month ago"
;       (test-portfolio ctx (t/local-date 2015 3 31) :by-account))))
; 
; (deftest get-a-portfolio-report-by-commodity
;   (let [ctx (realize portfolio-context)]
;     (testing "most recent"
;       (test-portfolio ctx (t/local-date 2015 4 30) :by-commodity))
;     (testing "1 month ago"
;       (test-portfolio ctx (t/local-date 2015 3 31) :by-commodity))))
