(ns clj-money.reports-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.edn :as edn]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql.ref]
            [clj-money.models.ref]
            [clj-money.models :as models]
            [clj-money.core]
            [clj-money.dates :as dates]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-budget
                                            find-account
                                            find-accounts
                                            find-commodity
                                            find-commodities]]
            [clj-money.factories.user-factory]
            [clj-money.trading :as trading]
            [clj-money.reports :as reports]
            [clj-money.models.accounts :as accounts]
            [clj-money.test-helpers :refer [reset-db]])
  (:import [java.io PushbackReader FileReader]))

(use-fixtures :each reset-db)

(defn- strip-accounts
  [items]
  (map (fn [item]
         (-> item
             (update-in-if [:items] strip-accounts)
             (dissoc item :id)))
       items))

(def ^:private base-context
  [(factory :user, {:user/email "john@doe.com"})
   #:entity{:user "john@doe.com"
            :name "Personal"}
   #:commodity{:symbol "USD"
               :type :currency
               :name "US Dollar"
               :entity "Personal"}])

(def ^:private report-context
  (conj base-context
        #:account{:name "Checking"
                  :entity "Personal"
                  :type :asset}
        #:account{:name "Credit Card"
                  :entity "Personal"
                  :type :liability}
        #:account{:name "Salary"
                  :entity "Personal"
                  :type :income}
        #:account{:name "Rent"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "Groceries"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "Taxes"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "FIT"
                  :entity "Personal"
                  :type :expense
                  :parent "Taxes"}
        #:account{:name "Social Security"
                  :entity "Personal"
                  :type :expense
                  :parent "Taxes"}
        #:account{:name "Medicare"
                  :entity "Personal"
                  :type :expense
                  :parent "Taxes"}
        ; salary
        #:transaction{:transaction-date (t/local-date 2016 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 724M}
                              #:transaction-item{:action :debit
                                                 :account "FIT"
                                                 :quantity 200M}
                              #:transaction-item{:action :debit
                                                 :account "Social Security"
                                                 :quantity 62M}
                              #:transaction-item{:action :debit
                                                 :account "Medicare"
                                                 :quantity 15M}
                              #:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1001M}]}
        #:transaction{:transaction-date (t/local-date 2016 1 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 725M}
                              #:transaction-item{:action :debit
                                                 :account "FIT"
                                                 :quantity 200M}
                              #:transaction-item{:action :debit
                                                 :account "Social Security"
                                                 :quantity 62M}
                              #:transaction-item{:action :debit
                                                 :account "Medicare"
                                                 :quantity 15M}
                              #:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1002M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 726M}
                              #:transaction-item{:action :debit
                                                 :account "FIT"
                                                 :quantity 200M}
                              #:transaction-item{:action :debit
                                                 :account "Social Security"
                                                 :quantity 62M}
                              #:transaction-item{:action :debit
                                                 :account "Medicare"
                                                 :quantity 15M}
                              #:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1003M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 1004M}
                              #:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1004M}]}

        ; groceries
        #:transaction{:transaction-date (t/local-date 2016 1 3)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 100M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 100M}]}
        #:transaction{:transaction-date (t/local-date 2016 1 10)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 100M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 100M}]}
        #:transaction{:transaction-date (t/local-date 2016 1 17)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 100M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 100M}]}
        #:transaction{:transaction-date (t/local-date 2016 1 24)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 100M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 100M}]}
        #:transaction{:transaction-date (t/local-date 2016 1 31)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 100M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 100M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 7)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 101M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 101M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 14)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 101M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 101M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 21)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 101M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 101M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 28)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 101M}
                              #:transaction-item{:action :credit
                                                 :account "Credit Card"
                                                 :quantity 101M}]}
        ; rent
        #:transaction{:transaction-date (t/local-date 2016 1 4)
                      :entity "Personal"
                      :description "Landlord"
                      :items [#:transaction-item{:action :debit
                                                 :account "Rent"
                                                 :quantity 700M}
                              #:transaction-item{:action :credit
                                                 :account "Checking"
                                                 :quantity 700M}]}
        #:transaction{:transaction-date (t/local-date 2016 2 4)
                      :entity "Personal"
                      :description "Landlord"
                      :items [#:transaction-item{:action :debit
                                                 :account "Rent"
                                                 :quantity 700M}
                              #:transaction-item{:action :credit
                                                 :account "Checking"
                                                 :quantity 700M}]}))

(deftest create-an-income-statement
  (with-context report-context
    (is (seq-of-maps-like? [{:report/caption "Income"
                             :report/value 2003M
                             :report/style :header}
                            {:report/caption "Salary"
                             :report/value 2003M
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "Expense"
                             :report/value 1754M
                             :report/style :header}
                            {:report/caption "Groceries"
                             :report/value 500M
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "Rent"
                             :report/value 700M
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "Taxes"
                             :report/value 554M
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "FIT"
                             :report/value 400M
                             :report/style :data
                             :report/depth 1}
                            {:report/caption "Medicare"
                             :report/value 30M
                             :report/style :data
                             :report/depth 1}
                            {:report/caption "Social Security"
                             :report/value 124M
                             :report/style :data
                             :report/depth 1}
                            {:report/caption "Net"
                             :report/value 249M
                             :report/style :summary}]
                           (reports/income-statement (models/find (find-entity "Personal"))
                                                     (t/local-date 2016 1 1)
                                                     (t/local-date 2016 1 31)))
        "The return is a report data structure with income and expense accounts")))

(deftest create-a-balance-sheet-report
  (with-context report-context
    (is (seq-of-maps-like? [{:report/caption "Asset"
                             :report/value 749M
                             :report/style :header}
                            {:report/caption "Checking"
                             :report/value 749M
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "Liability"
                             :report/value 500M
                             :report/style :header}
                            {:report/caption "Credit Card"
                             :report/value 500M
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "Equity"
                             :report/value 249M
                             :report/style :header}
                            {:report/caption "Retained Earnings"
                             :report/value 249M ;  2,003 - 1,754 : Same as the Net line in the income statement
                             :report/style :data
                             :report/depth 0}
                            {:report/caption "Liabilities + Equity"
                             :report/value 749M
                             :report/style :summary}]
                           (reports/balance-sheet (models/find (find-entity "Personal"))
                                                  (t/local-date 2016 1 31)))
        "The report include assets, liabilities, and equity totals")))

; (def ^:private commodities-context
;   (-> report-context
;       (update-in [:accounts] #(concat % [{:name "IRA"
;                                           :type :asset}
;                                          {:name "LT Gains"
;                                           :type :income}
;                                          {:name "ST Gains"
;                                           :type :income}
;                                          {:name "LT Losses"
;                                           :type :expense}
;                                          {:name "ST Losses"
;                                           :type :expense}]))
;       (update-in [:transactions] #(conj % {:transaction-date (t/local-date 2016 1 2)
;                                            :description "Retirement savings"
;                                            :items [#:transaction-item{:action :credit
;                                                     :account "Checking"
;                                                     :quantity 1000M}
;                                                    #:transaction-item{:action :debit
;                                                     :account "IRA"
;                                                     :quantity 1000M}]}))
;       (update-in [:commodities] #(concat % [{:name "Apple, Inc."
;                                              :symbol "AAPL"
;                                              :type :stock
;                                              :exchange :nasdaq}
;                                             {:name "Microsoft Corp"
;                                              :symbol "MSFT"
;                                              :type :stock
;                                              :exchange :nasdaq}
;                                             {:name "General Electric Co."
;                                              :symbol "GE"
;                                              :type :stock
;                                              :exchange :nyse}]))
;       (assoc :prices [{:trade-date (t/local-date 2017 2 1)
;                        :price 20M
;                        :commodity "AAPL"}
;                       {:trade-date (t/local-date 2017 2 1)
;                        :price 5M
;                        :commodity "MSFT"}])))
; 
; (deftest balance-sheet-report-with-commodities
;   (let [context (realize commodities-context)
;         ira (find-account context "IRA")
;         commodity (find-commodity context "AAPL")
;         _ (trading/buy {:account (:id ira)
;                         :commodity (:id commodity)
;                         :shares 100M
;                         :value 500M
;                         :trade-date (t/local-date 2016 3 2)})
;         report (strip-accounts
;                 (reports/balance-sheet (-> context :entities first)
;                                        (t/local-date 2017 3 2)))
;         expected [{:caption "Asset"
;                    :value 3279M
;                    :style :header}
;                   {:caption "Checking"
;                    :value 779M
;                    :style :data
;                    :depth 0}
;                   {:caption "IRA"
;                    :value 2500M
;                    :style :data
;                    :depth 0}
;                   {:caption "AAPL"
;                    :value 2000M
;                    :style :data
;                    :depth 1}
;                   {:caption "Liability"
;                    :value 904M
;                    :style :header}
;                   {:caption "Credit Card"
;                    :value 904M
;                    :style :data
;                    :depth 0}
;                   {:caption "Equity"
;                    :value 2375M
;                    :style :header}
;                   {:caption "Retained Earnings"
;                    :value 875M
;                    :style :data
;                    :depth 0}
;                   {:caption "Unrealized Gains"
;                    :value 1500M
;                    :style :data
;                    :depth 0}
;                   {:caption "Liabilities + Equity"
;                    :value 3279M
;                    :style :summary}]]
;     (is (= expected report) "The report contains the correct data")))
; 
; (def commodities-account-summary-context
;   (assoc commodities-context
;          :trades [{:type :buy
;                    :account "IRA"
;                    :commodity "GE"
;                    :shares 100M
;                    :value 1000M
;                    :trade-date (t/local-date 2015 1 1)}
;                   {:type :sell
;                    :account "IRA"
;                    :commodity "GE"
;                    :shares 100M
;                    :value 2000M
;                    :trade-date (t/local-date 2015 12 20)
;                    :lt-capital-gains-account "LT Gains"
;                    :st-capital-gains-account "ST Gains"
;                    :lt-capital-loss-account  "LT Losses"
;                    :st-capital-loss-account "ST Losses"}
;                   {:type :buy
;                    :account "IRA"
;                    :commodity "AAPL"
;                    :shares 50M
;                    :value 500M
;                    :trade-date (t/local-date 2016 3 2)}
;                   {:type :buy
;                    :account "IRA"
;                    :commodity "MSFT"
;                    :shares 50M
;                    :value 500M
;                    :trade-date (t/local-date 2016 3 2)}]))
; 
; (deftest create-a-commodities-account-summary
;   (with-context commodities-account-summary-context
;     (let [ira (accounts/find-by {:name "IRA"})
;           [aapl msft] (find-commodities "AAPL"
;                                         "MSFT")
;           actual (reports/commodities-account-summary ira
;                                                       (t/local-date 2017 3 2))
;           expected [{:caption "Apple, Inc. (AAPL)"
;                      :commodity (:id aapl)
;                      :shares 50M
;                      :price 20M
;                      :cost 500M
;                      :value 1000M
;                      :gain 500M
;                      :style :data}
;                     {:caption "Microsoft Corp (MSFT)"
;                      :commodity (:id msft)
;                      :shares 50M
;                      :price 5M
;                      :cost 500M
;                      :value 250M
;                      :gain -250M
;                      :style :data}
;                     {:caption "Cash"
;                      :style :data
;                      :value 1000M}
;                     {:caption "Total"
;                      :cost 1000M
;                      :value 2250M
;                      :gain 250M
;                      :style :summary}]]
;       (is (= expected actual) "The report contains the correct data"))))
; 
; (def ^:private budget-fixtures
;   (edn/read {:readers {'local-date dates/unserialize-local-date
;                        'repeat (fn [[n x]]
;                                  (repeat n x))}}
;             (PushbackReader.
;               (FileReader. "resources/fixtures/reports_test/budget.edn"))))
; 
; (def ^:private budget-report-context
;   (merge base-context (:context budget-fixtures)))
; 
; (def ^:private  expected-budget
;   (:expected budget-fixtures))
; 
; (deftest create-a-budget-report
;   (let [context (realize budget-report-context)
;         actual (update-in
;                 (reports/budget (-> context :budgets first)
;                                 {:as-of (t/local-date 2016 2 29)})
;                 [:items]
;                 strip-accounts)]
;     (is (= expected-budget actual) "The function produces the correct data")))
; 
; (def ^:private  expected-tagged-budget
;   (:expected-tagged budget-fixtures))
; 
; (deftest create-a-budget-report-grouped-by-tags
;   (let [ctx (realize budget-report-context)
;         actual (-> ctx
;                    (find-budget "2016")
;                    (reports/budget {:as-of (t/local-date 2016 2 29)
;                                     :tags [:tax :mandatory :discretionary]})
;                    (update-in [:items] strip-accounts))]
;     (is (= expected-tagged-budget actual) "The function produces the correct data")))
; 
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
