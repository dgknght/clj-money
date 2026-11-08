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

(def ^:private closed-accounts-portfolio-context
  (conj fixtures/portfolio-context
        #:account{:name "Old IRA"
                  :type :asset
                  :system-tags #{:trading}
                  :entity "Personal"}
        #:trade{:date (t/local-date 2015 4 15)
                :entity "Personal"
                :type :sale
                :account "401k"
                :commodity "MSFT"
                :shares 300M
                :value 1200M})) ; $4.00/share

(deftest portfolio-excludes-closed-accounts
  (with-context closed-accounts-portfolio-context
    (let [entity (entities/find (find-entity "Personal"))]
      (doseq [aggregate [:by-account :by-commodity]]
        (testing (name aggregate)
          (let [captions (->> (portfolio/report {:aggregate aggregate
                                                 :entity entity
                                                 :as-of (t/local-date 2015 4 30)})
                              (map :report/caption)
                              set)]
            (is (not-any? #(re-find #"MSFT" %) captions)
                "A commodity whose shares have all been sold is excluded")
            (is (not (captions "Old IRA"))
                "A trading account with no balance is excluded")
            (is (some #(re-find #"AAPL" %) captions)
                "A commodity still held is included")))))))

(defn- captions-with-parents
  "Pairs each row's caption with the captions of the rows it's nested under"
  [records]
  (let [captions (into {} (map (juxt :id :report/caption)) records)]
    (map (fn [{:report/keys [caption parent-ids]}]
           [caption (mapv captions parent-ids)])
         records)))

(deftest portfolio-rows-identify-their-parents
  (with-context fixtures/portfolio-context
    (let [entity (entities/find (find-entity "Personal"))
          report #(portfolio/report {:aggregate %
                                     :entity entity
                                     :as-of (t/local-date 2015 4 30)})]
      (testing "by account"
        (is (= [["401k" []]
                ["Cash" ["401k"]]
                ["MSFT" ["401k"]]
                ["4/1/2015" ["401k" "MSFT"]]
                ["2/1/2015" ["401k" "MSFT"]]
                ["IRA" []]
                ["Cash" ["IRA"]]
                ["AAPL" ["IRA"]]
                ["3/1/2015" ["IRA" "AAPL"]]
                ["2/1/2015" ["IRA" "AAPL"]]
                ["Total" []]]
               (captions-with-parents (report :by-account)))
            "Each row lists the account and commodity rows it's nested under"))
      (testing "by commodity"
        (is (= [["USD" []]
                ["401k" ["USD"]]
                ["IRA" ["USD"]]
                ["AAPL" []]
                ["3/1/2015" ["AAPL"]]
                ["2/1/2015" ["AAPL"]]
                ["MSFT" []]
                ["4/1/2015" ["MSFT"]]
                ["2/1/2015" ["MSFT"]]
                ["Total" []]]
               (captions-with-parents (report :by-commodity)))
            "Each row lists the commodity row it's nested under")))))
