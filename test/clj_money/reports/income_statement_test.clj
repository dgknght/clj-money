(ns clj-money.reports.income-statement-test
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
            [clj-money.reports.income-statement :as income-statement]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(deftest create-an-income-statement
  (with-context fixtures/report-context
    (is (seq-of-maps-like? fixtures/expected-income-statement
                           (income-statement/report (entities/find (find-entity "Personal"))
                                                    (t/local-date 2016 1 1)
                                                    (t/local-date 2016 1 31)))
        "The return is a report data structure with income and expense accounts")))
