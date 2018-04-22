(ns clj-money.import-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :refer [go-loop <! <!! chan]]
            [clojure.string :as s]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.util :refer [file-ext
                                    file-name]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff
                                            find-accounts]]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.imports :as imports]
            [clj-money.models.lots :as lots]
            [clj-money.models.prices :as prices]
            [clj-money.reports :as reports]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(defn- nil-chan []
  (let [c (chan)]
    (go-loop [v (<! c)]
             (recur (<! c)))
    c))

(defn- path->image
  [path]
  (let [ext (file-ext path)
        content-type (s/replace ext #"\.gz$" "")
        filename (file-name path)]
    {:body (-> path
               io/input-stream
               read-bytes)
     :content-type (format "application/%s" content-type)
     :original-filename filename}))

(def images
  {:gnucash (map path->image ["resources/fixtures/sample.gnucash"])
   :edn     (map path->image ["resources/fixtures/sample_0.edn.gz"
                              "resources/fixtures/sample_1.edn.gz"])})

(defn- import-context
  [source-type]
  {:users [(factory :user, {:email "john@doe.com"})]
   :images (source-type images)
   :imports [{:entity-name "Personal"
              :image-ids (map :original-filename (source-type images))}]})

(def expected-updates
  (concat [{:commodity {:total 2
                        :imported 1}
            :account {:total 9}
            :transaction {:total 6}}]
          (map (fn [i] {:commodity {:total 2
                                    :imported 1}
                        :account {:total 9
                                  :imported (+ 1 i)}
                        :transaction {:total 6}})
               (range 4))
          (map (fn [i] {:commodity {:total 2
                                    :imported 1}
                        :account {:total 9
                                  :imported 4}
                        :transaction {:total 6
                                      :imported (+ 1 i)}})
               (range 6))
          [{:commodity {:total 2
                        :imported 1}
            :account {:total 9
                      :imported 4}
            :transaction {:total 6
                          :imported 6}
            :finished true}]))

(def ^:private expected-inc-stmt
  [{:caption "Income"
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
    :style :summary}])

(def ^:private expected-bal-sheet
  [{:caption "Asset"
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
                             :style :summary}])

(def ^:private expected-accounts
  [{:name "Checking"
    :type :asset
    :commodity-id "USD"
    :balance 1810M}
   {:name "Credit Card"
    :type :liability
    :commodity-id "USD"
    :balance 100M}
   {:name "Groceries"
    :type :expense
    :commodity-id "USD"
    :balance 290M}
   {:name "Salary"
    :type :income
    :commodity-id "USD"
    :balance 2000M}])

(defn- test-import
  [context]
  (let [user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        updates (atom [])
        progress-chan (chan)
        _ (go-loop [p (<! progress-chan)]
                   (swap! updates #(conj % p))
                   (recur (<! progress-chan)))
        entity (import-data storage-spec imp progress-chan)
        actual-accounts (->> {:entity-id (:id entity)}
                             (accounts/search storage-spec)
                             (map #(dissoc % :created-at
                                           :updated-at
                                           :id
                                           :entity-id
                                           :commodity)))
        expected-accounts (->> expected-accounts
                               (map #(assoc % :tags #{}))
                               (map #(update-in %
                                                [:commodity-id]
                                                (fn [sym]
                                                  (->> {:entity-id (:id entity)
                                                        :symbol sym}
                                                       (commodities/search storage-spec)
                                                       first
                                                       :id)))))
        actual-inc-stmt (reports/income-statement storage-spec
                                                  (:id entity))
        actual-bal-sheet (reports/balance-sheet storage-spec
                                                (:id entity))]
    (is entity "It returns a value")
    (is (= "Personal" (:name entity)) "It returns the new entity")
    (pprint-diff expected-accounts actual-accounts)
    (is (= expected-accounts actual-accounts)
        "The correct accounts are created")
    (pprint-diff expected-inc-stmt actual-inc-stmt)
    (is (= expected-inc-stmt actual-inc-stmt)
        "The income statement is correct after import")
    (pprint-diff expected-bal-sheet actual-bal-sheet)
    (is (= expected-bal-sheet actual-bal-sheet)
        "The balance sheet is correct after import")
    (pprint-diff expected-updates @updates)
    (is (= expected-updates @updates)
        "The import record is updated at each insert")))

(deftest import-a-simple-gnucash-file
  (test-import
    (serialization/realize
      storage-spec
      (import-context :gnucash))))

(deftest import-a-simple-edn-file
  (test-import
    (serialization/realize
      storage-spec
      (import-context :edn))))

(def gnucash-budget-sample
  (io/input-stream "resources/fixtures/budget_sample.gnucash"))

(def import-budget-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :images [{:body (read-bytes gnucash-budget-sample)
             :content-type "application/gnucash"
             :original-filename "budget_sample.gnucash"}]
   :imports [{:entity-name "Personal"
              :image-ids ["budget_sample.gnucash"]}]})

(deftest receive-updates-asynchronously
  (let [context (serialization/realize
                  storage-spec
                  (import-context :gnucash))
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        channel (chan)
        updates (atom [])]
    (go-loop [p (<! channel)]
             (swap! updates #(conj % p))
             (recur (<! channel)))
    (import-data storage-spec imp channel)
    (pprint-diff (set expected-updates) (set @updates))
    (is (= (set expected-updates) (set @updates))
        "The import record is updated at each insert")
    (shutdown-agents)))

(deftest import-a-budget
  (let [context (serialization/realize storage-spec import-budget-context)
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        result (import-data storage-spec imp (nil-chan))
        entity (first (entities/select storage-spec (:id user)))
        [salary groceries] (->> {:entity-id (:id entity)}
                                (accounts/search storage-spec)
                                (sort #(compare (:name %2) (:name %1))))
        actual (-> (->> {:entity-id (:id entity)}
                        (budgets/search storage-spec)
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
    (pprint-diff expected actual)
    (is (= expected actual) "The budget exists after import with correct values")))

(def gnucash-commodities-sample
  (io/input-stream "resources/fixtures/sample_with_commodities.gnucash"))

(def ^:private commodities-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :images [{:body (read-bytes gnucash-commodities-sample)
             :content-type "application/gnucash"
             :original-filename "sample_with_commodities.gnucash"}]
   :imports [{:entity-name "Personal"
              :image-ids ["sample_with_commodities.gnucash"]}]})

(def ^:private expected-lots
  [{:purchase-date (t/local-date 2015 1 17)
    :shares-purchased 100M
    :shares-owned 100M
    :purchase-price 10M}])

(def ^:private expected-prices
  #{{:trade-date (t/local-date 2015 1 17)
     :price 10M}
    {:trade-date (t/local-date 2015 1 30)
     :price 12M}})

(deftest import-commodities
  (let [context (serialization/realize storage-spec commodities-context)
        user (-> context :users first)
        image (-> context :images first)
        imp (-> context :imports first)
        result (import-data storage-spec imp (nil-chan))
        entity (first (entities/select storage-spec (:id user)))
        account (->> {:entity-id (:id entity)}
                     (accounts/search storage-spec)
                     (filter #(= "401k" (:name %)))
                     first)
        lots (lots/search storage-spec {:account-id (:id account)})
        actual-lots (map #(dissoc % :id
                                    :commodity-id
                                    :account-id
                                    :created-at
                                    :updated-at)
                         lots)
        aapl (first (commodities/search storage-spec
                                        {:entity-id (:id entity)
                                         :symbol "AAPL"}
                                        {:limit 1}))
        prices  (prices/search storage-spec {:commodity-id (:id aapl)
                                             :trade-date [:between
                                                          (t/local-date 2015 1 1)
                                                          (t/local-date 2015 12 31)]})
        actual-prices (->> prices
                           (map #(dissoc % :id
                                           :commodity-id
                                           :created-at
                                           :updated-at))
                           (into #{}))]
    (pprint-diff expected-lots actual-lots)
    (is (= expected-lots actual-lots) "The correct lots are present after import")
    (pprint-diff expected-prices actual-prices)
    (is (= expected-prices, actual-prices) "The correct prices are present after import")))

(def gnucash-ext-commodities-sample
  (io/input-stream "resources/fixtures/sample_with_commodities_ext.gnucash"))

(def ^:private ext-commodities-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :images [{:body (read-bytes gnucash-ext-commodities-sample)
             :content-type "application/gnucash"
             :original-filename "sample_with_commodities_ext.gnucash"}]
   :imports [{:entity-name "Personal"
              :image-ids ["sample_with_commodities_ext.gnucash"]}]})

(deftest import-commodities-with-extended-actions
  (let [context (serialization/realize storage-spec ext-commodities-context)
        entity (import-data storage-spec
                            (-> context :imports first)
                            (nil-chan))
        [ira four-o-one-k] (map #(accounts/find-by storage-spec
                                                   {:name %
                                                    :entity-id (:id entity)})
                                ["IRA" "401k"])
        aapl (commodities/find-by storage-spec {:entity-id (:id entity)
                                                :symbol "AAPL"})]

    (testing "lots are adjusted"
      (let [
            lots (lots/search storage-spec {:commodity-id (:id aapl)})
            expected-lots [{:purchase-date (t/local-date 2015 1 17)
                            :shares-purchased 200M
                            :shares-owned 200M
                            :purchase-price 5M ; originally purchased 100 shares at $10/share
                            :commodity-id (:id aapl)
                            :account-id (:id ira)}]
            actual-lots (map #(dissoc % :updated-at :created-at :id) lots)]
        (pprint-diff expected-lots actual-lots)
        (is (= expected-lots actual-lots)
            "The commodity has the correct lots after import")))

    (testing "accounts are tagged correctly"
      (is (:trading (:tags ira))
          "The IRA account has the correct tags")
      (is (:trading (:tags four-o-one-k))
          "The 401k account has the correct tags"))

    (testing "transactions are created correctly"
      (let [ira-aapl (accounts/find-by storage-spec {:parent-id (:id ira)
                                                     :commodity-id (:id aapl)})
            expected-ira-items [{:transaction-date (t/local-date 2015 3 2)
                                 :description "Transfer 100 shares of AAPL"
                                 :index 0
                                 :action :debit
                                 :account-id (:id ira-aapl)
                                 :amount 100M
                                 :balance 100M
                                 :value 1000M}
                                {:transaction-date (t/local-date 2015 4 1)
                                 :description "Split shares of AAPL 2 for 1"
                                 :index 1
                                 :action :debit
                                 :account-id (:id ira-aapl)
                                 :amount 100M
                                 :balance 200M
                                 :value 0M}]
            actual-ira-items (->> {:account-id (:id ira-aapl)}
                                  (transactions/search-items storage-spec)
                                  (map #(dissoc % :created-at
                                                  :updated-at
                                                  :id
                                                  :memo
                                                  :transaction-id
                                                  :negative
                                                  :polarized-amount
                                                  :reconciliation-status
                                                  :reconciliation-id
                                                  :reconciled?)))]
        (pprint-diff expected-ira-items actual-ira-items)
        (is (= expected-ira-items actual-ira-items)
            "The IRA account has the correct items")))

    (testing "account balances are calculated correctly"
      (is (= 0M (:balance four-o-one-k)) "All shares have been transfered out of 401k")
      (is (= 200M (:balance ira)) "Shares have been transfered into IRA")))) ; TODO Adjust this to account for value, not shares
