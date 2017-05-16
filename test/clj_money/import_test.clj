(ns clj-money.import-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.reports :as reports]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash :as gnucash]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def import-context
  {:users [(factory :user, {:email "john@doe.com"})] })

(def gnucash-sample
  (io/input-stream "resources/fixtures/sample.gnucash"))

(deftest import-a-simple-file
  (let [context (serialization/realize storage-spec import-context)
        user (-> context :users first)
        result (import-data storage-spec
                            user
                            "Personal"
                            gnucash-sample
                            :gnucash)
        entity (-> storage-spec (entities/select (:id user)) first)
        expected-inc-stmt [{:caption "Income"
                            :value 2000M
                            :style :header}
                           {:caption "Salary"
                            :value 2000M
                            :style :data
                            :depth 0}
                           {:caption "Expense"
                            :value 290M
                            :style :header}
                           {:caption "Groceries"
                            :value 290M
                            :style :data
                            :depth 0}
                           {:caption "Net"
                            :value 1710M
                            :style :summary}]
        actual-inc-stmt (reports/income-statement storage-spec
                                                  (:id entity)
                                                  (t/local-date 1999 1 1)
                                                  (t/local-date 9999 12 31))
        expected-bal-sheet [{:caption "Asset"
                             :value 1810.00M
                             :style :header}
                            {:caption "Checking"
                             :value 1810.00M
                             :style :data
                             :depth 0}
                            {:caption "Liability"
                             :value 100.00M
                             :style :header}
                            {:caption "Credit Card"
                             :value 100.00M
                             :style :data
                             :depth 0}
                            {:caption "Equity"
                             :value 1710.00M
                             :style :header}
                            {:caption "Retained Earnings"
                             :value 1710.00M
                             :style :data
                             :depth 0}
                            {:caption "Unrealized Gains"
                             :value 0M
                             :style :data
                             :depth 0}
                            {:caption "Liabilities + Equity"
                             :value 1810.00M
                             :style :summary}]
        actual-bal-sheet (reports/balance-sheet storage-spec
                                                (:id entity)
                                                (t/local-date 9999 12 31))]
    (is (= expected-inc-stmt actual-inc-stmt)
        "The income statement is correct after import")
    (is (= expected-bal-sheet actual-bal-sheet)
        "The balance sheet is correct after import")))
