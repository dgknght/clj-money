(ns clj-money.import-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer [deftest is use-fixtures testing assert-expr do-report]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go-loop <! chan]]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.core :refer [safe-nth]]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.io :refer [read-bytes
                                  file-ext
                                  file-name]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-account
                                            find-import]]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models :as models]
            [clj-money.reports :as reports]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]))

(use-fixtures :each reset-db)

(defn- nil-chan []
  (let [c (chan)]
    (go-loop [_ (<! c)]
      (recur (<! c)))
    c))

(defn- path->image
  [path]
  (let [ext (file-ext path)
        content-type (s/replace ext #"\.gz$" "")
        filename (file-name path)]
    #:image{:body (-> path
                      io/input-stream
                      read-bytes)
            :user "john@doe.com"
            :content-type (format "application/%s" content-type)
            :original-filename filename}))

(def images
  {:gnucash (mapv path->image ["resources/fixtures/sample.gnucash"])
   :ext     (mapv path->image ["resources/fixtures/sample_with_commodities_ext.gnucash"])
   :edn     (mapv path->image ["resources/fixtures/sample_0.edn.gz"
                               "resources/fixtures/sample_1.edn.gz"])
   :sched   (mapv path->image ["resources/fixtures/scheduled_transactions.gnucash"])})

(def ^:private base-context
  [(factory :user, {:user/email "john@doe.com"})])

#_(defn- make-context
  [source-type]
  (concat [(factory :user, {:user/email "john@doe.com"})]
          (images source-type)
          [#:import{:entity-name "Personal"
                    :user "john@doe.com"
                    :images (map :original-filename images)
                    :options {:lt-capital-gains-account-id "Investment/Long-Term Gains"}}]))

(def ^:private expected-inc-stmt
  [{:report/caption "Income"
    :report/value 2000M
    :report/style :header}
   {:report/caption "Salary"
    :report/value 2000M
    :report/style :data
    :report/depth 0}
   {:report/caption "Expense"
    :report/value 290M
    :report/style :header}
   {:report/caption "Groceries"
    :report/value 290M
    :report/style :data
    :report/depth 0}
   {:report/caption "Net"
    :report/value 1710M
    :report/style :summary}])

(def ^:private expected-bal-sheet
  [{:report/caption "Asset"
    :report/value 1810.00M
    :report/style :header}
   {:report/caption "Checking"
    :report/value 1810.00M
    :report/style :data
    :report/depth 0}
   {:report/caption "Liability"
    :report/value 100.00M
    :report/style :header}
   {:report/caption "Credit Card"
    :report/value 100.00M
    :report/style :data
    :report/depth 0}
   {:report/caption "Equity"
    :report/value 1710.00M
    :report/style :header}
   {:report/caption "Retained Earnings"
    :report/value 1710.00M
    :report/style :data
    :report/depth 0}
   {:report/caption "Liabilities + Equity"
    :report/value 1810.00M
    :report/style :summary}])

(defn- expected-accounts []
  (let [usd (util/->model-ref
              (models/find-by
                {:commodity/symbol "USD"}))]
    [#:account{:name "Checking"
               :type :asset
               :commodity usd
               :quantity 1810M
               :value 1810M}
     #:account{:name "Credit Card"
               :type :liability
               :commodity usd
               :quantity 100M
               :value 100M}
     #:account{:name "Groceries"
               :type :expense
               :commodity usd
               :quantity 290M
               :value 290M}
     #:account{:name "Salary"
               :type :income
               :commodity usd
               :quantity 2000M
               :value 2000M}]))

(defn- execute-import
  [imp]
  (let [updates (atom [])
        progress-chan (chan)
        _ (go-loop [p (<! progress-chan)]
                   (when p
                     (swap! updates #(conj % p))
                     (recur (<! progress-chan))))
        {:keys [entity wait]} (import-data imp progress-chan {:atomic? false})] ; TODO: can we continue to have atomic imports?
    @wait
    {:entity entity
     :updates @updates}))

(defmethod assert-expr 'includes-progress-notification?
  [msg form]
  (let  [k (safe-nth form 1)
         expected (safe-nth form 2)
         coll  (safe-nth form 3)]
    `(let  [found# (->> ~coll
                        (filter #(= ~expected
                                    (get-in % [~k])))
                        first)]
       (do-report  (merge  {:expected ~expected
                            :message ~msg
                            :actual found#}
                          (if found#
                            {:type :pass}
                            {:type :fail
                             :actual (->> ~coll
                                          (map #(get-in % [~k]))
                                          (filter identity))}))))))

(defn- includes-progress-records
  [updates]
  (is (includes-progress-notification?
        :commodity
        {:total 2
         :completed 0}
        updates)
      "The initial commodity progress is reported")
  (is (includes-progress-notification?
        :commodity
        {:total 2
         :completed 2}
        updates)
      "The final commodity progress is reported")
  (is (includes-progress-notification?
        :account
        {:total 9
         :completed 0}
        updates)
      "The initial account progress is reported")
  (is (includes-progress-notification?
        :account
        {:total 9
         :completed 9}
        updates)
      "The final account progress is reported")
  (is (includes-progress-notification?
        :transaction
        {:total 6
         :completed 0}
        updates)
      "The initial transaction progress is reported")
  (is (includes-progress-notification?
        :transaction
        {:total 6
         :completed 6}
        updates)
      "The final transaction progress is reported")
  (is (includes-progress-notification?
        :account-balance
        {:total 4
         :completed 0}
        updates)
      "The initial account balance progress is reported")
  (is (includes-progress-notification?
        :account-balance
        {:total 4
         :completed 4}
        updates)
      "The final account balance progress is reported"))

(defn- test-import []
  (let [imp (find-import "Personal")
        {:keys [entity] :as result} (execute-import imp)]
    (testing "the return value"
      (is (comparable? {:entity/name "Personal"}
                       entity)
          "It returns the new entity")
      (includes-progress-records (:updates result)))
    (testing "models"
      (is (comparable? #:entity{:name "Personal"
                                :settings #:settings{:default-commodity (util/->model-ref (models/find-by #:commodity{:symbol "USD"
                                                                                                                      :entity entity}))
                                                     :earliest-transaction-date (t/local-date 2015 1 1)
                                                     :latest-transaction-date (t/local-date 2015 1 18)}}
                       (models/find entity)))
      "The entity can be retrieved"
      (is (seq-of-maps-like? (expected-accounts)
                             (models/select {:account/entity entity}
                                            {:sort [:account/name]}))
          "The accounts are created")
      (is (seq-of-maps-like? [#:reconciliation{:account (util/->model-ref (models/find-by {:account/name "Checking"}))
                                               :status :completed
                                               :end-of-period (t/local-date 2015 1 15)
                                               :balance 800M}]
                             (models/select
                               (db/model-type
                                 {:account/entity entity}
                                 :reconciliation)))
          "Reconciliations can be retrieved"))
    (testing "reports"
      (is (seq-of-maps-like? expected-inc-stmt
                             (reports/income-statement entity
                                                       (t/local-date 2015 1 1)
                                                       (t/local-date 2017 12 31)))
          "An income statement can be produced")
      (is (seq-of-maps-like? expected-bal-sheet
                             (reports/balance-sheet entity
                                                    (t/local-date 2017 12 31)))
          "A balance sheet can be produced"))))

(def ^:private gnucash-context
  (conj base-context
        #:image{:body (-> "resources/fixtures/sample.gnucash"
                          io/input-stream
                          read-bytes)
                :user "john@doe.com"
                :content-type "application/gnucash"
                :original-filename "sample.gnucash"}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["sample.gnucash"]
                 :options {:lt-capital-gains-account-id "Investment/Long-Term Gains"}}))

(deftest import-a-simple-gnucash-file
  (with-context gnucash-context
    (test-import)))

(def ^:private edn-context
  (conj base-context
        #:image{:body (-> "resources/fixtures/sample_0.edn.gz"
                          io/input-stream
                          read-bytes)
                :user "john@doe.com"
                :content-type "application/edn"
                :original-filename "sample_0.edn.gz"}
        #:image{:body (-> "resources/fixtures/sample_1.edn.gz"
                          io/input-stream
                          read-bytes)
                :user "john@doe.com"
                :content-type "application/edn"
                :original-filename "sample_1.edn.gz"}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["sample_0.edn.gz" "sample_1.edn.gz"]
                 :options {:lt-capital-gains-account-id "Investment/Long-Term Gains"}}))

(deftest import-a-simple-edn-file
  (with-context edn-context
    (test-import)))

(def ^:private ext-context
  [])

(deftest import-with-entity-settings
  (with-context ext-context
    (let [imp (find-import "Personal")
          result (execute-import imp)
          entity (models/find (:entity result))] ; the entity is returned immediately with the promise which the import goes on in the background, so we have to look it up again to get the latest version
      (is (util/model-ref? (get-in entity [:settings :lt-capital-gains-account]))
          "The long-term capital gains account id is set correctly")
      (is (util/model-ref? (get-in entity [:settings :st-capital-gains-account]))
          "The short-term capital gains account id is set correctly")
      (is (util/model-ref? (get-in entity [:settings :lt-capital-loss-account]))
          "The long-term capital losses account id is set correctly")
      (is (util/model-ref? (get-in entity [:settings :lt-capital-loss-account]))
          "The short-term capital losses account id is set correctly"))))

(defn- gnucash-budget-sample []
  (with-open [input (io/input-stream "resources/fixtures/budget_sample.gnucash")]
    (read-bytes input)))

(def import-budget-context
  (conj base-context
        #:image{:body (gnucash-budget-sample)
                :user "john@doe.com"
                :content-type "application/gnucash"
                :original-filename "budget_sample.gnucash"}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["budget_sample.gnucash"]}))

(defn- account-ref
  [name]
  (-> {:account/name name}
      models/find-by
      util/->model-ref))

(deftest import-a-budget
  (with-context import-budget-context
    (let [imp (find-import "Personal")
          {:keys [entity wait]} (import-data imp (nil-chan))]
      @wait
      (is (seq-of-maps-like? [#:budget{:name "2017"
                                       :entity (util/->model-ref entity)
                                       :period :month
                                       :period-count 12
                                       :start-date (t/local-date 2017 1 1)
                                       :end-date (t/local-date 2017 12 31)
                                       :items [#:budget-item{:account (account-ref "Salary")
                                                             :periods (repeat 12 1000M)}
                                               #:budget-item{:account (account-ref "Bonus")
                                                             :periods  [0M 0M 0M 0M 0M 0M 0M 0M 0M 0M 0M 800M]}
                                               #:budget-item{:account (account-ref "Groceries")
                                                             :periods [200M 200M 250M
                                                                       250M 275M 275M
                                                                       200M 200M 250M
                                                                       250M 275M 275M]}]}]
                             (models/select {:budget/entity entity}))
          "The budget can be retrieved"))))

; (def gnucash-commodities-sample
;   (io/input-stream "resources/fixtures/sample_with_commodities.gnucash"))
;
; (def ^:private commodities-context
;   {:users [(factory :user, {:email "john@doe.com"})]
;    :images [{:body (read-bytes gnucash-commodities-sample)
;              :content-type "application/gnucash"
;              :original-filename "sample_with_commodities.gnucash"}]
;    :imports [{:entity-name "Personal"
;               :image-ids ["sample_with_commodities.gnucash"]}]})
;
; (def ^:private expected-lots
;   [{:purchase-date (t/local-date 2015 1 17)
;     :shares-purchased 100M
;     :shares-owned 100M
;     :purchase-price 10M}])
;
; (def ^:private expected-prices
;   #{{:trade-date (t/local-date 2015 1 17)
;      :price 10M}
;     {:trade-date (t/local-date 2015 1 30)
;      :price 12M}})
;
; (deftest import-commodities
;   (let [context (realize commodities-context)
;         imp (-> context :imports first)
;         {:keys [entity]} (import-data imp (nil-chan) {:atomic? true})
;         account (accounts/find-by {:entity-id (:id entity)
;                                    :name "401k"})
;         lots (lots/search {:account-id (:id account)})
;         actual-lots (map #(dissoc % :id
;                                   :commodity-id
;                                   :account-id
;                                   :created-at
;                                   :updated-at)
;                          lots)
;         aapl (commodities/find-by {:entity-id (:id entity)
;                                    :symbol "AAPL"}
;                                   {:limit 1})
;         prices  (prices/search {:commodity-id (:id aapl)
;                                 :trade-date [:between
;                                              (t/local-date 2015 1 1)
;                                              (t/local-date 2015 12 31)]})
;         actual-prices (->> prices
;                            (map #(dissoc % :id
;                                          :commodity-id
;                                          :created-at
;                                          :updated-at))
;                            (into #{}))]
;     (is (= expected-lots actual-lots) "The correct lots are present after import")
;     (is (= expected-prices, actual-prices) "The correct prices are present after import")))
;
; (def gnucash-ext-commodities-sample
;   (io/input-stream "resources/fixtures/sample_with_commodities_ext.gnucash"))
;
; (def ^:private ext-commodities-context
;   {:users [(factory :user, {:email "john@doe.com"})]
;    :images [{:body (read-bytes gnucash-ext-commodities-sample)
;              :content-type "application/gnucash"
;              :original-filename "sample_with_commodities_ext.gnucash"}]
;    :imports [{:entity-name "Personal"
;               :image-ids ["sample_with_commodities_ext.gnucash"]}]})
;
; (deftest import-commodities-with-extended-actions
;   (let [context (realize ext-commodities-context)
;         {:keys [entity]} (import-data (-> context :imports first)
;                                       (nil-chan)
;                                       {:atomic? true})
;         [ira four-o-one-k] (map #(accounts/find-by {:name %
;                                                     :entity-id (:id entity)})
;                                 ["IRA" "401k"])
;         aapl (commodities/find-by {:entity-id (:id entity)
;                                    :symbol "AAPL"})]
;
;     (testing "lots are adjusted"
;       (let [lots (lots/search {:commodity-id (:id aapl)})
;             expected-lots [{:purchase-date (t/local-date 2015 1 17)
;                             :shares-purchased 200M
;                             :shares-owned 100M ; originally purchased 100 shares, they split 2 for 1, then we sold 100
;                             :purchase-price 5M ; originally purchased 100 shares at $10/share
;                             :commodity-id (:id aapl)
;                             :account-id (:id ira)}]
;             actual-lots (map #(dissoc % :updated-at :created-at :id) lots)]
;         (is (= expected-lots actual-lots)
;             "The commodity has the correct lots after import")))
;
;     (testing "accounts are tagged correctly"
;       (is (:trading (:system-tags ira))
;           "The IRA account has the correct system tags")
;       (is (:trading (:system-tags four-o-one-k))
;           "The 401k account has the correct system tags"))
;
;     (testing "transactions are created correctly"
;       (let [ira-aapl (accounts/find-by {:parent-id (:id ira)
;                                         :commodity-id (:id aapl)})
;             inv-exp (accounts/find-by {:name "Investment Expenses"
;                                        :entity-id (:id entity)})
;             expected-ira-items [{:transaction-date (t/local-date 2015 3 2)
;                                  :description "Transfer 100 shares of AAPL"
;                                  :index 0
;                                  :action :debit
;                                  :account-id (:id ira-aapl)
;                                  :quantity 100M
;                                  :balance 100M
;                                  :value 1000M}
;                                 {:transaction-date (t/local-date 2015 4 1)
;                                  :description "Split shares of AAPL 2 for 1"
;                                  :index 1
;                                  :action :debit
;                                  :account-id (:id ira-aapl)
;                                  :quantity 100M
;                                  :balance 200M
;                                  :value 0M}
;                                 {:description "Sell 100 shares of AAPL at 6.000"
;                                  :index 2
;                                  :value 500M ; this is reflecting the original value, not the sale value...is that right?
;                                  :account-id (:id ira-aapl)
;                                  :balance 100M
;                                  :transaction-date (t/local-date 2015 5 1)
;                                  :action :credit
;                                  :quantity 100M}]
;             actual-ira-items (map #(dissoc % :created-at
;                                            :updated-at
;                                            :id
;                                            :memo
;                                            :transaction-id
;                                            :negative
;                                            :polarized-quantity
;                                            :reconciliation-status
;                                            :reconciliation-id
;                                            :reconciled?)
;                                   (transactions/search-items
;                                    {:account-id (:id ira-aapl)
;                                     :transaction-date [:between> (t/local-date 2015 1 1) (t/local-date 2016 1 1)]}
;                                    {:sort [:index]}))
;             expected-fee-items [{:transaction-date (t/local-date 2015 5 1)
;                                  :description "Sell 100 shares of AAPL at 6.000"
;                                  :value 10M
;                                  :quantity 10M
;                                  :account-id (:id inv-exp)
;                                  :balance 10M
;                                  :action :debit
;                                  :index 0}]
;             actual-fee-items (->> {:account-id (:id inv-exp)
;                                    :transaction-date [:between> (t/local-date 2015 1 1) (t/local-date 2016 1 1)]}
;                                   transactions/search-items
;                                   (map #(dissoc %
;                                                 :created-at
;                                                 :updated-at
;                                                 :id
;                                                 :memo
;                                                 :transaction-id
;                                                 :negative
;                                                 :polarized-quantity
;                                                 :reconciliation-status
;                                                 :reconciliation-id
;                                                 :reconciled?)))]
;         (is (seq-of-maps-like? expected-fee-items actual-fee-items)
;             "The Investment Expenses account has the correct items")
;         (is (seq-of-maps-like? expected-ira-items actual-ira-items)
;             "The IRA account has the correct items")))
;
;     (testing "account balances are calculated correctly"
;       (is (= 0M (:quantity four-o-one-k)) "All shares have been transfered out of 401k")
;       (is (= 590M (:quantity ira)) "Shares have been transfered into IRA"))))
;
; (defmulti comparable
;   (fn [x]
;     (if (sequential? x)
;       :collection
;       (storage/tag x))))
;
; (defmethod comparable :collection
;   [coll]
;   (map comparable coll))
;
; (defn- strip-db-attr
;   [m]
;   (dissoc m :id :created-at :updated-at))
;
; (defmethod comparable ::models/scheduled-transaction
;   [sched-tran]
;   (-> sched-tran
;       strip-db-attr
;       (update-in [:items] (fn [items]
;                             (map (comp strip-db-attr
;                                        #(dissoc % :scheduled-transaction-id))
;                                  items)))))
;
; (deftest import-scheduled-transactions
;   (let [ctx (realize (import-context :sched))
;         imp (find-import ctx "Personal")
;         {:keys [entity updates]} (execute-import imp)
;         checking (accounts/find-by {:entity-id (:id entity)
;                                     :name "Checking"})
;         salary (accounts/find-by {:entity-id (:id entity)
;                                   :name "Salary"})]
;     (is (= {:total 1 :completed 1}
;            (:scheduled-transaction (last updates)))
;         "The progress is updated for the scheduled transactions")
;     (is (= [{:entity-id (:id entity)
;              :description "Paycheck"
;              :memo nil
;              :start-date (t/local-date 2016 1 15)
;              :end-date (t/local-date 2018 12 31)
;              :enabled true
;              :date-spec {:days [:friday]}
;              :last-occurrence nil
;              :interval-type :week
;              :interval-count 2
;              :items [{:action :debit
;                       :account-id (:id checking)
;                       :quantity 1000M
;                       :memo nil}
;                      {:action :credit
;                       :account-id (:id salary)
;                       :quantity 1000M
;                       :memo nil}]}]
;            (comparable (sched-trans/search {:entity-id (:id entity)}))))))
