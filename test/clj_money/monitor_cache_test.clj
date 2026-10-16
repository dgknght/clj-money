(ns clj-money.monitor-cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as a]
            [integrant.core :as ig]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.reports :as rpt]
            [clj-money.monitor-cache :as cache]))

(def ^:private groceries {:id 101 :account/name "Groceries"})
(def ^:private dining {:id 102 :account/name "Dining"})

(defn- with-cache*
  [opts f]
  (let [publisher (ig/init-key ::entities/publisher {})
        c (ig/init-key ::cache/cache (assoc opts :publisher publisher))
        computed (atom [])]
    (try
      (with-redefs [rpt/monitor (fn [account & _]
                                  (swap! computed conj (:id account))
                                  {:report/caption (:account/name account)
                                   :report/n (count @computed)})
                    ; Produce (103) is a child of Groceries
                    rpt/monitored-account-ids (fn [{:keys [id]}]
                                                (if (= 101 id)
                                                  #{101 103}
                                                  #{id}))]
        (with-fixed-time "2016-01-07T12:00:00Z"
          (f {:cache c
              :publisher publisher
              :computed computed})))
      (finally
        (ig/halt-key! ::cache/cache c)
        (ig/halt-key! ::entities/publisher publisher)))))

(defmacro ^:private with-cache
  [[sym opts] & body]
  `(with-cache* ~opts (fn [~sym] ~@body)))

(defn- trx-change
  [& account-ids]
  [nil {:transaction/description "Kroger"
        :transaction/items (map (fn [id]
                                  {:transaction-item/account {:id id}})
                                account-ids)}])

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

(deftest a-monitor-is-computed-once
  (with-cache [{:keys [cache computed]} {}]
    (is (= {:report/caption "Groceries" :report/n 1}
           (cache/fetch cache groceries)))
    (is (= {:report/caption "Groceries" :report/n 1}
           (cache/fetch cache groceries))
        "The cached monitor is returned")
    (is (= [101] @computed)
        "The monitor is computed only once")))

(deftest a-monitor-is-computed-every-time-without-a-cache
  (with-cache [{:keys [computed]} {}]
    (cache/fetch nil groceries)
    (cache/fetch nil groceries)
    (is (= [101 101] @computed))))

(deftest a-propagated-transaction-evicts-the-monitors-for-its-accounts
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (cache/fetch cache groceries)
    (cache/fetch cache dining)
    (propagated! ctx (trx-change 101 201))
    (cache/fetch cache groceries)
    (cache/fetch cache dining)
    (is (= [101 102 101] @computed)
        "Only the monitor for an account of the transaction is computed again")))

(deftest a-propagated-transaction-in-a-child-account-evicts-the-parent-monitor
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (cache/fetch cache groceries)
    (propagated! ctx (trx-change 103 201))
    (cache/fetch cache groceries)
    (is (= [101 101] @computed))))

(deftest a-deleted-transaction-evicts-the-monitors-for-its-accounts
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (cache/fetch cache groceries)
    (propagated! ctx (reverse (trx-change 101 201)))
    (cache/fetch cache groceries)
    (is (= [101 101] @computed))))

(deftest an-unpropagated-transaction-does-not-evict-monitors
  ; the monitor would be computed from balances that haven't been updated
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (cache/fetch cache groceries)
    (published! ctx (trx-change 101 201))
    (cache/fetch cache groceries)
    (is (= [101] @computed))))

(deftest other-propagated-changes-do-not-evict-monitors
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (cache/fetch cache groceries)
    (propagated! ctx [nil {:price/price 10M}])
    (cache/fetch cache groceries)
    (is (= [101] @computed))))

(deftest budget-and-account-changes-evict-every-monitor
  (doseq [change [[nil {:budget/name "2016"}]
                  [{:budget-item/periods [1M]} {:budget-item/periods [2M]}]
                  [{:account/name "Food"} nil]]]
    (testing (pr-str change)
      (with-cache [{:keys [cache computed] :as ctx} {}]
        (cache/fetch cache groceries)
        (cache/fetch cache dining)
        (published! ctx change)
        (cache/fetch cache groceries)
        (cache/fetch cache dining)
        (is (= [101 102 101 102] @computed))))))

(deftest a-monitor-computed-during-an-eviction-is-not-cached
  (with-cache [{:keys [cache computed] :as ctx} {}]
    (let [monitor rpt/monitor]
      (with-redefs [rpt/monitor (fn [& args]
                                  (let [result (apply monitor args)]
                                    (when (= 1 (count @computed))
                                      (propagated! ctx (trx-change 101)))
                                    result))]
        (cache/fetch cache groceries)
        (cache/fetch cache groceries)))
    (is (= [101 101] @computed)
        "The monitor computed from the stale state is not cached")))

(deftest a-monitor-expires
  (with-cache [{:keys [cache computed]} {:ttl-minutes 0}]
    (cache/fetch cache groceries)
    (cache/fetch cache groceries)
    (is (= [101 101] @computed))))

(deftest a-monitor-is-cached-for-one-day
  (with-cache [{:keys [cache computed]} {}]
    (cache/fetch cache groceries)
    (with-fixed-time "2016-01-08T12:00:00Z"
      (cache/fetch cache groceries))
    (is (= [101 101] @computed))
    (is (= 1 (count (:entries @(:state cache))))
        "The monitors for other days are removed")))
