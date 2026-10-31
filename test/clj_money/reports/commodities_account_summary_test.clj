(ns clj-money.reports.commodities-account-summary-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
            [clj-money.core]
            [clj-money.test-context :refer [with-context
                                            find-account]]
            [clj-money.reports.commodities-account-summary :as commodities-account-summary]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(deftest create-a-commodities-account-summary
  (with-context fixtures/commodities-account-summary-context
    (is (seq-of-maps-like? (fixtures/expected-commodities-account-summary)
                           (-> "IRA"
                               find-account
                               entities/find
                               (commodities-account-summary/report (t/local-date 2017 3 2))))
        "The report contains the commodities in the account and their current values based on most recent available price")))
