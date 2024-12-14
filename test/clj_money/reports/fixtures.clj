(ns clj-money.reports.fixtures
  (:require [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [find-commodity]]
            [clj-money.util :as util]))


(def  base-context
  [(factory :user, {:user/email "john@doe.com"})
   #:entity{:user "john@doe.com"
            :name "Personal"}
   #:commodity{:symbol "USD"
               :type :currency
               :name "US Dollar"
               :entity "Personal"}])

(def  report-context
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

(def expected-income-statement
  [{:report/caption "Income"
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
    :report/style :summary}])

(def expected-balance-sheet
  [{:report/caption "Asset"
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
    :report/style :summary}])

(def commodities-context
  (conj report-context
        #:commodity{:name "Apple, Inc."
                    :entity "Personal"
                    :symbol "AAPL"
                    :type :stock
                    :exchange :nasdaq}
        #:commodity{:name "Microsoft Corp"
                    :entity "Personal"
                    :symbol "MSFT"
                    :type :stock
                    :exchange :nasdaq}
        #:commodity{:name "General Electric Co."
                    :entity "Personal"
                    :symbol "GE"
                    :type :stock
                    :exchange :nyse}
        #:price{:trade-date (t/local-date 2017 2 1)
                :price 20M
                :commodity "AAPL"}
        #:price{:trade-date (t/local-date 2017 2 1)
                :price 5M
                :commodity "MSFT"}
        #:account{:name "IRA"
                  :entity "Personal"
                  :type :asset}
        #:account{:name "LT Gains"
                  :entity "Personal"
                  :type :income}
        #:account{:name "ST Gains"
                  :entity "Personal"
                  :type :income}
        #:account{:name "LT Losses"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "ST Losses"
                  :entity "Personal"
                  :type :expense}
        #:transaction{:transaction-date (t/local-date 2016 1 2)
                      :entity "Personal"
                      :description "Retirement savings"
                      :debit-account "IRA"
                      :credit-account "Checking"
                      :quantity 1000M}
        #:trade{:type :purchase
                :account "IRA"
                :commodity "AAPL"
                :shares 100M
                :value 500M
                :date (t/local-date 2016 3 2)}))

(def expected-balance-sheet-with-commodities
  [{:report/caption "Asset"
    :report/value 3279M
    :report/style :header}
   {:report/caption "Checking"
    :report/value 779M
    :report/style :data
    :report/depth 0}
   {:report/caption "IRA"
    :report/value 2500M
    :report/style :data
    :report/depth 0}
   {:report/caption "AAPL"
    :report/value 2000M
    :report/style :data
    :report/depth 1}
   {:report/caption "Liability"
    :report/value 904M
    :report/style :header}
   {:report/caption "Credit Card"
    :report/value 904M
    :report/style :data
    :report/depth 0}
   {:report/caption "Equity"
    :report/value 2375M
    :report/style :header}
   {:report/caption "Retained Earnings"
    :report/value 875M
    :report/style :data
    :report/depth 0}
   {:report/caption "Unrealized Gains"
    :report/value 1500M
    :report/style :data
    :report/depth 0}
   {:report/caption "Liabilities + Equity"
    :report/value 3279M
    :report/style :summary}])

(def commodities-account-summary-context
  (conj commodities-context
        #:trade{:type :purchase
                :account "IRA"
                :commodity "GE"
                :shares 100M
                :value 1000M
                :date (t/local-date 2015 1 1)}
        #:trade{:type :sell
                :account "IRA"
                :commodity "GE"
                :shares 100M
                :value 2000M
                :date (t/local-date 2015 12 20)
                :lt-capital-gains-account "LT Gains"
                :st-capital-gains-account "ST Gains"
                :lt-capital-loss-account  "LT Losses"
                :st-capital-loss-account "ST Losses"}
        #:trade{:type :purchase
                :account "IRA"
                :commodity "AAPL"
                :shares 50M
                :value 500M
                :date (t/local-date 2016 3 3)}
        #:trade{:type :purchase
                :account "IRA"
                :commodity "MSFT"
                :shares 50M
                :value 500M
                :date (t/local-date 2016 3 3)}))

(def budget-context
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
        #:account{:name "Dining"
                  :entity "Personal"
                  :user-tags #{:discretionary}
                  :type :expense}
        #:account{:name "Clothes"
                  :entity "Personal"
                  :user-tags #{:discretionary}
                  :type :expense}
        #:account{:name "Rent"
                  :entity "Personal"
                  :user-tags #{:mandatory}
                  :type :expense}
        #:account{:name "Groceries"
                  :entity "Personal"
                  :user-tags #{:mandatory}
                  :type :expense}
        #:account{:name "Taxes"
                  :entity "Personal"
                  :user-tags #{:tax}
                  :type :expense}
        #:account{:name "FIT"
                  :entity "Personal"
                  :user-tags #{:tax}
                  :type :expense
                  :parent "Taxes"}
        #:account{:name "Social Security"
                  :entity "Personal"
                  :user-tags #{:tax}
                  :type :expense
                  :parent "Taxes"}
        #:account{:name "Medicare"
                  :entity "Personal"
                  :user-tags #{:tax}
                  :type :expense
                  :parent "Taxes"}
        #:budget{:name "2016"
                 :entity "Personal"
                 :start-date (t/local-date 2016 01 01)
                 :period :month
                 :period-count 12
                 :items [#:budget-item{:account "Salary"
                                       :periods (repeat 12 2000M)}
                         #:budget-item{:account "FIT"
                                       :periods (repeat 12 400M)}
                         #:budget-item{:account "Social Security"
                                       :periods (repeat 12 134M)}
                         #:budget-item{:account "Medicare"
                                       :periods (repeat 12 30M)}
                         #:budget-item{:account "Rent"
                                       :periods (repeat 12 700M)}
                         #:budget-item{:account "Dining"
                                       :periods (repeat 12 200M)}
                         #:budget-item{:account "Groceries"
                                       :periods (repeat 12 450M)}]}
        ; salary
        #:transaction{:transaction-date (t/local-date 2016 01 01)
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
        #:transaction{:transaction-date (t/local-date 2016 01 15)
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
        #:transaction{:transaction-date (t/local-date 2016 02 01)
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
        #:transaction{:transaction-date (t/local-date 2016 02 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1004M
                      :debit-account "Checking"
                      :credit-account "Salary"}

        ; groceries
        #:transaction{:transaction-date (t/local-date 2016 01 03)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 01 10)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 01 17)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 01 24)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 01 31)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 02 07)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 101M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 02 14)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 101M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 02 21)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 101M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}
        #:transaction{:transaction-date (t/local-date 2016 02 28)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 101M
                      :debit-account "Groceries"
                      :credit-account "Credit Card"}

        ; rent
        #:transaction{:transaction-date (t/local-date 2016 01 04)
                      :entity "Personal"
                      :description "Landlord"
                      :quantity 700M
                      :debit-account "Rent"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2016 02 04)
                      :entity "Personal"
                      :description "Landlord"
                      :quantity 700M
                      :debit-account "Rent"
                      :credit-account "Checking"}

        ; clothes
        #:transaction{:transaction-date (t/local-date 2016 01 07)
                      :entity "Personal"
                      :description "Trunk Club"
                      :quantity 321M
                      :debit-account "Clothes"
                      :credit-account "Checking"}))

(def expected-budget-report
  {:title "2016: January to February"
   :items [#:report{:caption "Income"
                    :style :header
                    :budget 4000M
                    :actual 4010M
                    :difference 10M
                    :percent-difference 0.0025M
                    :actual-per-period 2005M
                    :items [#:report{:caption "Salary"
                                     :style :data
                                     :depth 0
                                     :budget 4000M
                                     :actual 4010M
                                     :difference 10M
                                     :percent-difference 0.0025M
                                     :actual-per-period 2005M}]}
           #:report{:caption "Expense"
                    :style :header
                    :budget 3828M
                    :actual 3456M
                    :difference 372M
                    :percent-difference 0.09717868339M
                    :actual-per-period 1728M
                    :items [#:report{:caption "Clothes"
                                     :style :data
                                     :depth 0
                                     :budget 0M
                                     :actual 321M
                                     :difference -321M
                                     :percent-difference nil
                                     :actual-per-period 160.50M}
                            #:report{:caption "Groceries"
                                     :style :data
                                     :depth 0
                                     :budget 900M
                                     :actual 904M
                                     :difference -4M
                                     :percent-difference -0.0044444M
                                     :actual-per-period 452M}
                            #:report{:caption "Rent"
                                     :style :data
                                     :depth 0
                                     :budget 1400M
                                     :actual 1400M
                                     :difference 0M
                                     :percent-difference 0M
                                     :actual-per-period 700M}
                            #:report{:caption "Taxes"
                                     :style :data
                                     :depth 0
                                     :budget 0M
                                     :actual 0M
                                     :difference 0M
                                     :percent-difference nil
                                     :actual-per-period 0M
                                     :roll-up #:report{:budget 1128M ; 60 + 268 + 800
                                                       :actual 831M ; 45 + 186 + 600
                                                       :difference 297M
                                                       :percent-difference 0.2632978723M
                                                       :actual-per-period 415.5M}}
                            #:report{:caption "Taxes/Medicare"
                                     :style :data
                                     :depth 1
                                     :budget 60M
                                     :actual 45M
                                     :difference 15M
                                     :percent-difference 0.25M
                                     :actual-per-period 22.5M}
                            #:report{:caption "Taxes/Social Security"
                                     :style :data
                                     :depth 1
                                     :budget 268M
                                     :actual 186M
                                     :difference 82M
                                     :percent-difference 0.30597M
                                     :actual-per-period 93M}
                            #:report{:caption "Taxes/FIT"
                                     :style :data
                                     :depth 1
                                     :budget 800M
                                     :actual 600M
                                     :difference 200M
                                     :percent-difference 0.25M
                                     :actual-per-period 300M}
                            #:report{:caption "Dining"
                                     :style :data
                                     :depth 0
                                     :budget 400M
                                     :actual 0M
                                     :difference 400M
                                     :percent-difference 1M
                                     :actual-per-period 0M}]}
           #:report{:caption "Net"
                    :style :summary
                    :budget 172M
                    :actual 554M
                    :difference 382M
                    :percent-difference 2.220930233M
                    :actual-per-period 277M}
           ]})

(def expected-budget-report-by-tag
  {:title "2016: January to February"
   :items [{:caption "Income"
            :style :header
            :budget 4000M
            :actual 4010M
            :difference 10M
            :percent-difference 0.0025M
            :actual-per-period 2005M
            :items [{:caption "Salary"
                     :user-tags #{}
                     :style :data
                     :depth 0
                     :budget 4000M
                     :actual 4010M
                     :difference 10M
                     :percent-difference 0.0025M
                     :actual-per-period 2005M}]}
           {:caption "Tax"
            :style :header
            :budget 1128M
            :actual 831M
            :difference 297M
            :percent-difference 0.2632978723M
            :actual-per-period 415.5M
            :items [{:caption "Taxes"
                     :user-tags #{:tax}
                     :style :data
                     :depth 0
                     :budget 0M
                     :actual 0M
                     :difference 0M
                     :percent-difference nil
                     :actual-per-period 0M
                     :roll-up {:budget 1128M ; 60 + 268 + 800
                               :actual 831M ; 45 + 186 + 600
                               :difference 297M
                               :percent-difference 0.2632978723M
                               :actual-per-period 415.5M}}
                    {:caption "Taxes/Medicare"
                     :user-tags #{:tax}
                     :style :data
                     :depth 1
                     :budget 60M
                     :actual 45M
                     :difference 15M
                     :percent-difference 0.25M
                     :actual-per-period 22.5M}
                    {:caption "Taxes/Social Security"
                     :user-tags #{:tax}
                     :style :data
                     :depth 1
                     :budget 268M
                     :actual 186M
                     :difference 82M
                     :percent-difference 0.30597M
                     :actual-per-period 93M}
                    {:caption "Taxes/FIT"
                     :user-tags #{:tax}
                     :style :data
                     :depth 1
                     :budget 800M
                     :actual 600M
                     :difference 200M
                     :percent-difference 0.25M
                     :actual-per-period 300M}]}
           {:caption "Mandatory"
            :style :header
            :budget 2300M
            :actual 2304M
            :difference -4M
            :percent-difference -0.001739130435M
            :actual-per-period 1152M
            :items [{:caption "Groceries"
                     :user-tags #{:mandatory}
                     :style :data
                     :depth 0
                     :budget 900M
                     :actual 904M
                     :difference -4M
                     :percent-difference -0.0044444M
                     :actual-per-period 452M}
                    {:caption "Rent"
                     :user-tags #{:mandatory}
                     :style :data
                     :depth 0
                     :budget 1400M
                     :actual 1400M
                     :difference 0M
                     :percent-difference 0M
                     :actual-per-period 700M}]}
           {:caption "Discretionary"
            :style :header
            :budget 400M
            :actual 321M
            :difference 79M
            :percent-difference 0.1975M
            :actual-per-period 160.50M
            :items [{:caption "Clothes"
                     :user-tags #{:discretionary}
                     :style :data
                     :depth 0
                     :budget 0M
                     :actual 321M
                     :difference -321M
                     :percent-difference nil
                     :actual-per-period 160.50M}
                    {:caption "Dining"
                     :user-tags #{:discretionary}
                     :style :data
                     :depth 0
                     :budget 400M
                     :actual 0M
                     :difference 400M
                     :percent-difference 1M
                     :actual-per-period 0M}]}
           {:caption "Net"
            :style :summary
            :budget 172M
            :actual 554M
            :difference 382M
            :percent-difference 2.220930233M
            :actual-per-period 277M}]})

; AAPL
; purchased 100 shares at $ 5 ($500) on 2016-03-02
; purchased  50 shares at $10 ($500) on 2016-03-03
; latest price $20 on 2017-02-01
(defn expected-commodities-account-summary []
  [{:report/caption "Apple, Inc. (AAPL)"
    :report/commodity (util/->model-ref (find-commodity "AAPL"))
    :report/shares 150M
    :report/price 20M
    :report/cost 1000M
    :report/value 3000M
    :report/gain 2000M
    :report/style :data}
   {:report/caption "Microsoft Corp (MSFT)"
    :report/commodity (util/->model-ref (find-commodity "MSFT"))
    :report/shares 50M
    :report/price 5M
    :report/cost 500M
    :report/value 250M
    :report/gain -250M
    :report/style :data}
   {:report/caption "Cash"
    :report/style :data
    :report/value 500M}
   {:report/caption "Total"
    :report/cost 1500M
    :report/value 3750M
    :report/gain 1750M
    :report/style :summary}])
