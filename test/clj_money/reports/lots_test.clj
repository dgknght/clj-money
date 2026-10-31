(ns clj-money.reports.lots-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.core]
            [clj-money.test-context :refer [with-context
                                            find-account]]
            [clj-money.reports.lots :as lots]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(deftest get-a-lot-report
  (with-context fixtures/lot-report-context
    (is (seq-of-maps-like? fixtures/expected-lot-report
                           (lots/report (find-account "IRA")))
        "The report contains lot information grouped by commodity")))
