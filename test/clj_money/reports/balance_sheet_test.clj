(ns clj-money.reports.balance-sheet-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
            [clj-money.core]
            [clj-money.test-context :refer [with-context
                                            find-entity]]
            [clj-money.reports.balance-sheet :as balance-sheet]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(deftest create-a-balance-sheet-report
  (with-context fixtures/report-context
    (is (seq-of-maps-like? fixtures/expected-balance-sheet
                           (balance-sheet/report (entities/find (find-entity "Personal"))
                                                 (t/local-date 2016 1 31)))
        "The report include assets, liabilities, and equity totals")))


(deftest balance-sheet-report-with-commodities
  (with-context fixtures/commodities-context
    (is (seq-of-maps-like? fixtures/expected-balance-sheet-with-commodities
                           (balance-sheet/report (entities/find (find-entity "Personal"))
                                                 (t/local-date 2017 3 2)))
        "The balance sheet includes unrealized gains")))
