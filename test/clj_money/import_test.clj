(ns clj-money.import-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :refer [go <! <!! chan]]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.imports :as imports]
            [clj-money.models.lots :as lots]
            [clj-money.models.prices :as prices]
            [clj-money.reports :as reports]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash :as gnucash]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def gnucash-sample
  (io/input-stream "resources/fixtures/sample.gnucash"))

(def import-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :images [{:body (read-bytes gnucash-sample)
             :original-filename "sample.gnucash"}]
   :imports [{:entity-name "Personal"
              :image-id "sample.gnucash"}]})

(def expected-updates
  (concat [{:commodity {:total 1}}
           {:commodity {:total 1}
            :account {:total 9}}
           {:commodity {:total 1}
            :account {:total 9}
            :transaction {:total 6}}
           {:commodity {:total 1
                        :imported 1}
            :account {:total 9}
            :transaction {:total 6}}]
          (map (fn [i] {:commodity {:total 1
                                    :imported 1}
                        :account {:total 9
                                  :imported (+ 1 i)}
                        :transaction {:total 6}})
               (range 9))
          (map (fn [i] {:commodity {:total 1
                                    :imported 1}
                        :account {:total 9
                                  :imported 9}
                        :transaction {:total 6
                                      :imported (+ 1 i)}})
               (range 6))))

(deftest import-a-simple-file
  (let [context (serialization/realize storage-spec import-context)
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        updates (atom [])
        entity (import-data storage-spec imp (fn [p] (swap! updates #(conj % p))))
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
    (is entity "It returns a value")
    (is (= "Personal" (:name entity)) "It returns the new entity")
    (is (= expected-inc-stmt actual-inc-stmt)
        "The income statement is correct after import")
    (is (= expected-bal-sheet actual-bal-sheet)
        "The balance sheet is correct after import")
    (is (= expected-updates @updates)
        "The import record is updated at each insert")))

(def gnucash-budget-sample
  (io/input-stream "resources/fixtures/budget_sample.gnucash"))

(def import-budget-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :images [{:body (read-bytes gnucash-budget-sample)
             :original-filename "budget_sample.gnucash"}]
   :imports [{:entity-name "Personal"
              :image-id "budget_sample.gnucash"}]})

(deftest receive-updates-asynchronously
  (let [context (serialization/realize storage-spec import-context)
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        channel (chan)
        updates (atom [])]
    (go
      (while true
        (let [p (<! channel)]
          (swap! updates #(conj % p)))))
    (import-data storage-spec imp channel)
    (if-not (= (set expected-updates) (set @updates))
      (pprint {:expected expected-updates
               :actual @updates
               :diff (diff expected-updates @updates)}))
    (is (= (set expected-updates) (set @updates))
        "The import record is updated at each insert")
    (shutdown-agents)))

(deftest import-a-budget
  (let [context (serialization/realize storage-spec import-budget-context)
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        result (import-data storage-spec imp (fn [progress]))
        entity (first (entities/select storage-spec (:id user)))
        [salary groceries] (->> (:id entity)
                                (accounts/select-by-entity-id storage-spec)
                                (sort #(compare (:name %2) (:name %1))))
        actual (-> (->> (:id entity)
                        (budgets/select-by-entity-id storage-spec)
                        first)
                   (dissoc :id :updated-at :created-at)
                   (update-in [:items] (fn [items]
                                         (map (fn [item]
                                                (-> item
                                                    (dissoc :budget-id :id :created-at :updated-at)
                                                    (update-in [:periods] #(sort-by :index %))))
                                              items))))
        expected {:name "2017"
                  :entity-id (:id entity)
                  :period :month
                  :period-count 12
                  :start-date (t/local-date 2017 1 1)
                  :end-date (t/local-date 2017 12 31)
                  :items [{:account-id (:id salary)
                           :periods (repeat 12 1000M)}
                          {:account-id (:id groceries)
                           :periods [200M 200M 250M
                                     250M 275M 275M
                                     200M 200M 250M
                                     250M 275M 275M]}]}]
    (is (= expected actual) "The budget exists after import with correct values")))

(def gnucash-commodities-sample
  (io/input-stream "resources/fixtures/sample_with_commodities.gnucash"))

(def ^:private commodities-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :images [{:body (read-bytes gnucash-commodities-sample)
             :original-filename "sample_with_commodities.gnucash"}]
   :imports [{:entity-name "Personal"
              :image-id "sample_with_commodities.gnucash"}]})

(def ^:private expected-lots
  [{:purchase-date (t/local-date 2015 1 17)
    :shares-purchased 100M
    :shares-owned 100M}])

(def ^:private expected-prices
  [{:trade-date (t/local-date 2015 1 17)
    :price 10M}
   {:trade-date (t/local-date 2015 1 30)
    :price 12M}])

(deftest import-commodities
  (let [context (serialization/realize storage-spec commodities-context)
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        result (import-data storage-spec imp (fn [progress]))
        entity (first (entities/select storage-spec (:id user)))
        account (->> (:id entity)
                     (accounts/select-by-entity-id storage-spec)
                     (filter #(= "401k" (:name %)))
                     first)
        lots (lots/search storage-spec {:account-id (:id account)})
        actual-lots (map #(dissoc % :id
                                    :commodity-id
                                    :account-id
                                    :created-at
                                    :updated-at)
                         lots)
        prices  (prices/search storage-spec {:entity-id (:id entity)})
        actual-prices (map #(dissoc % :id
                                      :commodity-id
                                      :created-at
                                      :updated-at)
                           prices)]
    (is (= expected-lots actual-lots) "The correct lots are present after import")
    (is (= expected-prices, actual-prices) "The correct prices are present after import")))
