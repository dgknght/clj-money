(ns clj-money.reports.budget-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
            [clj-money.core]
            [clj-money.test-context :refer [with-context]]
            [clj-money.reports.budget :as budget]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(deftest create-a-budget-report
  (with-context fixtures/budget-context
    (let [report (budget/report (entities/find-by {:budget/name "2016"}
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
    (let [report (budget/report (entities/find-by {:budget/name "2016"}
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
    (let [report (budget/report (entities/find-by {:budget/name "2016"}
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
