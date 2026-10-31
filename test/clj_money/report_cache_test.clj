(ns clj-money.report-cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as a]
            [integrant.core :as ig]
            [java-time.api :as t]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.reports.balance-sheet :as balance-sheet]
            [clj-money.reports.income-statement :as income-statement]
            [clj-money.reports.budget :as budget-report]
            [clj-money.report-cache :as cache]))

(def ^:private personal {:id 101 :entity/name "Personal"})
(def ^:private business {:id 102 :entity/name "Business"})

(def ^:private budget-2016
  {:id 201
   :budget/name "2016"
   :budget/entity {:id 101}
   :budget/start-date (t/local-date 2016 1 1)
   :budget/end-date (t/local-date 2016 12 31)})

(defn- with-cache*
  [opts f]
  (let [publisher (ig/init-key ::entities/publisher {})
        c (ig/init-key ::cache/cache (assoc opts :publisher publisher))
        computed (atom [])
        compute (fn [report-type id]
                  (swap! computed conj [report-type id])
                  {:report report-type
                   :n (count @computed)})]
    (try
      (with-redefs [balance-sheet/report (fn [entity _as-of]
                                           (compute :balance-sheet (:id entity)))
                    income-statement/report (fn [entity _since _as-of]
                                              (compute :income-statement (:id entity)))
                    budget-report/report (fn [budget _opts]
                                           (compute :budget (:id budget)))]
        (with-fixed-time "2016-03-02T12:00:00Z"
          (f {:cache c
              :publisher publisher
              :computed computed})))
      (finally
        (ig/halt-key! ::cache/cache c)
        (ig/halt-key! ::entities/publisher publisher)))))

(defmacro ^:private with-cache
  [[sym opts] & body]
  `(with-cache* ~opts (fn [~sym] ~@body)))

(defn- trx
  [entity-id date]
  #:transaction{:description "Kroger"
                :entity {:id entity-id}
                :transaction-date date})

(defn- propagated!
  "Publishes the ::prop/propagated signal for the change, as the
  propagation subscription does once it has propagated the change."
  [{:keys [publisher cache]} change]
  (entities/signal publisher ::prop/propagated {::prop/change change})
  (is (cache/await-evictions cache)))

(defn- published!
  "Publishes the change, as entities/put-many does."
  [{:keys [publisher cache]} change]
  (a/put! (:ch publisher) change)
  (is (cache/await-evictions cache)))

(defn- fetch-all
  "Fetches a balance sheet as of 2016-01-31, an income statement for
  February 2016 and the budget report as of 2016-02-29."
  [cache]
  (cache/balance-sheet cache personal (t/local-date 2016 1 31))
  (cache/income-statement cache
                          personal
                          (t/local-date 2016 2 1)
                          (t/local-date 2016 2 29))
  (cache/budget cache budget-2016 {:as-of (t/local-date 2016 2 29)}))

(def ^:private all-reports
  [[:balance-sheet 101]
   [:income-statement 101]
   [:budget 201]])

(deftest a-report-is-computed-once
  (with-cache [{:keys [cache computed]} {}]
    (is (= {:report :balance-sheet :n 1}
           (cache/balance-sheet cache personal (t/local-date 2016 1 31))))
    (is (= {:report :balance-sheet :n 1}
           (cache/balance-sheet cache personal (t/local-date 2016 1 31)))
        "The cached report is returned")
    (is (= [[:balance-sheet 101]] @computed)
        "The report is computed only once")))

(deftest reports-are-cached-by-entity-and-date
  (with-cache [{:keys [cache computed]} {}]
    (cache/balance-sheet cache personal (t/local-date 2016 1 31))
    (cache/balance-sheet cache personal (t/local-date 2016 2 29))
    (cache/balance-sheet cache business (t/local-date 2016 1 31))
    (cache/income-statement cache
                            personal
                            (t/local-date 2016 1 1)
                            (t/local-date 2016 1 31))
    (cache/budget cache budget-2016 {:as-of (t/local-date 2016 1 31)})
    (cache/budget cache budget-2016 {})
    (is (= [[:balance-sheet 101]
            [:balance-sheet 101]
            [:balance-sheet 102]
            [:income-statement 101]
            [:budget 201]
            [:budget 201]]
           @computed))))

(deftest a-report-is-computed-every-time-without-a-cache
  (with-cache [{:keys [computed]} {}]
    (fetch-all nil)
    (fetch-all nil)
    (is (= (concat all-reports all-reports)
           @computed))))

(deftest a-propagated-transaction-evicts-the-reports-that-include-its-date
  (doseq [[date expected] [[(t/local-date 2015 12 31) [[:balance-sheet 101]]]
                           [(t/local-date 2016 1 15) [[:balance-sheet 101]
                                                      [:budget 201]]]
                           [(t/local-date 2016 2 15) [[:income-statement 101]
                                                      [:budget 201]]]
                           [(t/local-date 2016 3 1) []]]]
    (testing (str date)
      (with-cache [{:keys [cache computed] :as ctx} {}]
        (fetch-all cache)
        (propagated! ctx [nil (trx 101 date)])
        (fetch-all cache)
        (is (= expected (drop 3 @computed)))))))

(deftest a-propagated-transaction-evicts-reports-for-its-date-before-the-change
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (fetch-all cache)
    (propagated! ctx [(trx 101 (t/local-date 2015 12 31))
                      (trx 101 (t/local-date 2016 3 1))])
    (fetch-all cache)
    (is (= [[:balance-sheet 101]] (drop 3 @computed)))))

(deftest a-deleted-transaction-evicts-the-reports-that-include-its-date
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (fetch-all cache)
    (propagated! ctx [(trx 101 (t/local-date 2015 12 31)) nil])
    (fetch-all cache)
    (is (= [[:balance-sheet 101]] (drop 3 @computed)))))

(deftest a-propagated-transaction-does-not-evict-reports-for-other-entities
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (fetch-all cache)
    (propagated! ctx [nil (trx 102 (t/local-date 2016 1 15))])
    (fetch-all cache)
    (is (= all-reports @computed))))

(deftest a-budget-report-without-a-date-is-evicted-for-any-date-in-the-budget
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (cache/budget cache budget-2016 {})
    (propagated! ctx [nil (trx 101 (t/local-date 2016 12 1))])
    (cache/budget cache budget-2016 {})
    (is (= [[:budget 201] [:budget 201]] @computed))))

(deftest an-unpropagated-transaction-does-not-evict-reports
  ; the report would be computed from balances that haven't been updated
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (fetch-all cache)
    (published! ctx [nil (trx 101 (t/local-date 2016 1 15))])
    (fetch-all cache)
    (is (= all-reports @computed))))

(deftest propagated-price-and-lot-changes-evict-every-report
  (doseq [change [[nil {:price/price 10M}]
                  [nil {:lot/shares-purchased 10M}]]]
    (testing (pr-str change)
      (with-cache [{:keys [cache computed] :as ctx} {}]
        (fetch-all cache)
        (propagated! ctx change)
        (fetch-all cache)
        (is (= (concat all-reports all-reports) @computed))))))

(deftest other-propagated-changes-do-not-evict-reports
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (fetch-all cache)
    (propagated! ctx [nil {:attachment/caption "Receipt"}])
    (fetch-all cache)
    (is (= all-reports @computed))))

(deftest account-budget-and-entity-changes-evict-every-report
  (doseq [change [[nil {:budget/name "2016"}]
                  [{:budget-item/periods [1M]} {:budget-item/periods [2M]}]
                  [{:account/name "Food"} nil]
                  [{:entity/name "Personal"} {:entity/name "Personal Stuff"}]]]
    (testing (pr-str change)
      (with-cache [{:keys [cache computed] :as ctx} {}]
        (fetch-all cache)
        (published! ctx change)
        (fetch-all cache)
        (is (= (concat all-reports all-reports) @computed))))))

(deftest a-report-computed-during-an-eviction-is-not-cached
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (let [report balance-sheet/report]
      (with-redefs [balance-sheet/report (fn [& args]
                                           (let [result (apply report args)]
                                             (when (= 1 (count @computed))
                                               (propagated! ctx [nil (trx 101 (t/local-date 2016 1 1))]))
                                             result))]
        (cache/balance-sheet cache personal (t/local-date 2016 1 31))
        (cache/balance-sheet cache personal (t/local-date 2016 1 31))))
    (is (= [[:balance-sheet 101] [:balance-sheet 101]] @computed)
        "The report computed from the stale state is not cached")))

(deftest a-report-expires
  (with-cache [{:keys [cache computed]} {:ttl-minutes 0}]
    (cache/balance-sheet cache personal (t/local-date 2016 1 31))
    (cache/balance-sheet cache personal (t/local-date 2016 1 31))
    (is (= [[:balance-sheet 101] [:balance-sheet 101]] @computed))
    (is (= 1 (count (:entries @(:state cache))))
        "Expired reports are removed")))

(deftest a-budget-report-without-a-date-is-cached-for-one-day
  (with-cache [{:keys [cache computed]} {}]
    (cache/budget cache budget-2016 {})
    (with-fixed-time "2016-03-03T12:00:00Z"
      (cache/budget cache budget-2016 {}))
    (is (= [[:budget 201] [:budget 201]] @computed))))
