(ns clj-money.reports.monitor-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
            [clj-money.core]
            [clj-money.test-context :refer [with-context]]
            [clj-money.reports.monitor :as monitor]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

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
                         (monitor/report groceries
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
                         (monitor/report groceries
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
                       (monitor/report account
                                       (t/local-date 2016 1 15)))
          "A zero-dollar period budget does not cause an error and yields a zero actual-percent"))))
