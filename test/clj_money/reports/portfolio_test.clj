(ns clj-money.reports.portfolio-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.reports.fixtures :as fixtures]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
            [clj-money.core]
            [clj-money.test-context :refer [with-context
                                            find-entity]]
            [clj-money.reports.portfolio :as portfolio]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- test-portfolio
  [as-of grouping]
  (let [entity (entities/find (find-entity "Personal"))
        expected (get-in fixtures/expected-portfolio-report
                         [grouping as-of])]
    (is (seq-of-maps-like? expected
                           (portfolio/report {:aggregate grouping
                                              :entity entity
                                              :as-of as-of}))
        "The data reflects the commodities owned and their values of the specified date")))

(deftest get-a-portfolio-report
  (with-context fixtures/portfolio-context
    (testing "most recent by account"
      (test-portfolio (t/local-date 2015 4 30) :by-account))
    (testing "1 month ago by account"
      (test-portfolio (t/local-date 2015 3 31) :by-account))
    (testing "most recent by commodity"
      (test-portfolio (t/local-date 2015 4 30) :by-commodity))
    (testing "1 month ago by commodity"
      (test-portfolio (t/local-date 2015 3 31) :by-commodity))))

(deftest portfolio-with-nested-trading-account
  (with-context fixtures/nested-portfolio-context
    (let [entity (entities/find (find-entity "Personal"))
          result (portfolio/report {:aggregate :by-account
                                    :entity entity
                                    :as-of (t/local-date 2015 4 30)})]
      (is (seq (remove #(= :summary (:report/style %)) result))
          "Trading accounts nested under a parent appear in the portfolio report"))))

(deftest portfolio-with-cash-only-nested-trading-account
  (with-context fixtures/cash-only-nested-portfolio-context
    (let [entity (entities/find (find-entity "Personal"))
          result (portfolio/report {:aggregate :by-account
                                    :entity entity
                                    :as-of (t/local-date 2015 4 30)})]
      (is (seq (remove #(= :summary (:report/style %)) result))
          "A cash-only trading account nested under a parent appears in the portfolio report"))))

(def ^:private same-day-lots-portfolio-context
  (conj fixtures/portfolio-context
        #:trade{:date (t/local-date 2015 2 1)
                :entity "Personal"
                :type :purchase
                :account "IRA"
                :commodity "AAPL"
                :shares 50M
                :value 500M}))

(deftest portfolio-rows-have-unique-ids
  (with-context same-day-lots-portfolio-context
    (let [entity (entities/find (find-entity "Personal"))]
      (doseq [aggregate [:by-account :by-commodity]]
        (testing (name aggregate)
          (let [ids (->> (portfolio/report {:aggregate aggregate
                                            :entity entity
                                            :as-of (t/local-date 2015 4 30)})
                         (remove #(= :summary (:report/style %)))
                         (map :id))]
            (is (not-any? nil? ids)
                "Every row has an id")
            (is (apply distinct? ids)
                "No two rows share an id, even lots purchased on the same day")))))))
