(ns clj-money.import-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer [deftest is use-fixtures testing assert-expr do-report]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go-loop <! chan]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.core :refer [safe-nth]]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.io :refer [read-bytes]]
            [clj-money.test-context :refer [with-context
                                            find-import]]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            account-ref]]
            [clj-money.accounts :refer [system-tagged?]]
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

(def ^:private base-context
  [(factory :user, {:user/email "john@doe.com"})])

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
  (conj base-context
        #:image{:body (-> "resources/fixtures/sample_with_commodities_ext.gnucash"
                          io/input-stream
                          read-bytes)
                :user "john@doe.com"
                :content-type "application/gnucash"
                :original-filename "sample_with_commodities_ext.gnucash"}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["sample_with_commodities_ext.gnucash"]
                 :options {:lt-capital-gains-account "Investment/Long-Term Gains"}}))

(deftest import-with-entity-settings
  (with-context ext-context
    (let [imp (find-import "Personal")
          result (execute-import imp)
          entity (models/find (:entity result))] ; the entity is returned immediately with the promise which the import goes on in the background, so we have to look it up again to get the latest version
      (is (util/model-ref? (get-in entity [:entity/settings
                                           :settings/lt-capital-gains-account]))
          "The long-term capital gains account id is set correctly")
      #_(is (util/model-ref? (get-in entity [:entity/settings
                                           :settings/st-capital-gains-account]))
          "The short-term capital gains account id is set correctly")
      #_(is (util/model-ref? (get-in entity [:entity/settings
                                           :settings/lt-capital-loss-account]))
          "The long-term capital losses account id is set correctly")
      #_(is (util/model-ref? (get-in entity [:entity/settings
                                           :settings/lt-capital-loss-account]))
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

(deftest import-a-budget
  (with-context import-budget-context
    (let [imp (find-import "Personal")
          {:keys [entity wait]} (import-data imp (nil-chan))]
      @wait
      (let [retrieved (models/select {:budget/entity entity})]
        (is (seq-of-maps-like? [#:budget{:name "2017"
                                         :entity (util/->model-ref entity)
                                         :period :month
                                         :period-count 12
                                         :start-date (t/local-date 2017 1 1)
                                         :end-date (t/local-date 2017 12 31)}]
                               retrieved)
            "The budget can be retrieved")
        (is (= #{#:budget-item{:account (account-ref "Salary")
                               :periods (repeat 12 1000M)}
                 #:budget-item{:account (account-ref "Bonus")
                               :periods  [0M 0M 0M 0M 0M 0M 0M 0M 0M 0M 0M 800M]}
                 #:budget-item{:account (account-ref "Groceries")
                               :periods [200M 200M 250M
                                         250M 275M 275M
                                         200M 200M 250M
                                         250M 275M 275M]}}
               (->> (:budget/items (first retrieved))
                    (map #(select-keys % [:budget-item/account
                                          :budget-item/periods]))
                    set))
            "The retrieved budget has the same items as the imported file")))))

(defn- gnucash-commodities-sample []
  (with-open [input (io/input-stream "resources/fixtures/sample_with_commodities.gnucash")]
    (read-bytes input)))

(def ^:private commodities-context
  (conj base-context
        #:image{:body (gnucash-commodities-sample)
                :user "john@doe.com"
                :content-type "application/gnucash"
                :original-filename "sample_with_commodities.gnucash"}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["sample_with_commodities.gnucash"]}))

(deftest import-commodities
  (with-context commodities-context
    (let [imp (find-import "Personal")
          {:keys [wait]} (import-data imp (nil-chan))]
      @wait
      (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2015 1 17)
                                    :shares-purchased 100M
                                    :shares-owned 100M
                                    :purchase-price 10M}]
                             (models/select #:lot{:account (account-ref "401k")}))
          "The lots can be retrieved")
      (is (seq-of-maps-like? [#:price{:trade-date (t/local-date 2015 1 17)
                                      :price 10M}
                              #:price{:trade-date (t/local-date 2015 1 30)
                                      :price 12M}]
                             (models/select
                               (db/model-type
                                 {:price/trade-date [:between>
                                                     (t/local-date 2015 1 1)
                                                     (t/local-date 2016 1 1)]
                                  :commodity/symbol "AAPL"}
                                 :price)
                               {:sort [[:price/trade-date :asc]]}))
          "The prices can be retrieved"))))

(deftest import-commodities-with-extended-actions
  (with-context ext-context
    (let [{:keys [entity wait]} (import-data (find-import "Personal")
                                             (nil-chan))
          _ @wait
          four-oh-one-k (models/find-by {:account/name "401k"})
          ira (models/find-by {:account/name "IRA"
                               :account/entity entity})
          aapl (models/find-by {:commodity/symbol "AAPL"
                                :commodity/entity entity})]
      (is (seq-of-maps-like?
            [#:lot{:purchase-date (t/local-date 2015 1 17)
                   :shares-purchased 200M
                   :shares-owned 100M ; originally purchased 100 shares, they split 2 for 1, then we sold 100
                   :purchase-price 5M ; originally purchased 100 shares at $10/share
                   :commodity (util/->model-ref aapl)
                   :account (util/->model-ref ira)}]
            (models/select {:lot/commodity aapl}))
          "The shares lost due to reverse split are subtracted from the lot")

      (testing "accounts are tagged correctly"
        (is (system-tagged? ira :trading)
            "The IRA account is tagged as a trading account")
        (is (system-tagged? four-oh-one-k :trading) 
            "The 401k account is tagged as a trading account"))

      (testing "transactions"
        (let [ira-aapl (models/find-by #:account{:parent ira
                                                 :commodity aapl})
              inv-exp (models/find-by #:account{:name "Investment Expenses"
                                                :entity entity})]
          (is (seq-of-maps-like?
                [#:transaction-item{:transaction-date (t/local-date 2015 5 1)
                                    ;:description "Sell 100 shares of AAPL at 6.000"
                                    :value 10M
                                    :quantity 10M
                                    :balance 10M
                                    :action :debit
                                    :index 0}]
                (models/select
                  #:transaction-item{:account inv-exp
                                     :transaction-date [:between>
                                                        (t/local-date 2015 1 1)
                                                        (t/local-date 2016 1 1)]}))
              "The Investment Expenses account receives items for the fees charged on trading actions")
          (is (seq-of-maps-like?
                [#:transaction-item{:transaction-date (t/local-date 2015 3 2)
                                    ;:description "Transfer 100 shares of AAPL"
                                    :index 0
                                    :action :debit
                                    :quantity 100M
                                    :balance 100M
                                    :value 1000M}
                 #:transaction-item{:transaction-date (t/local-date 2015 4 1)
                                    ;:description "Split shares of AAPL 2 for 1"
                                    :index 1
                                    :action :debit
                                    :quantity 100M
                                    :balance 200M
                                    :value 0M}
                 #:transaction-item{:transaction-date (t/local-date 2015 5 1)
                                    ;:description "Sell 100 shares of AAPL at 6.000"
                                    :index 2
                                    :action :credit
                                    :quantity 100M
                                    :value 500M ; this is reflecting the original value, not the sale value...is that right?
                                    :balance 100M}]
                (models/select
                  #:transaction-item{:account ira-aapl
                                     :transaction-date [:between>
                                                        (t/local-date 2015 1 1)
                                                        (t/local-date 2016 1 1)]}
                  {:sort [:transaction-item/index]}))
              "The IRA account receives items for the transfer, split and sale actions")))

      (testing "accounts"
        (is (= 0M (:account/quantity four-oh-one-k))
            "All shares have been transfered out of 401k")
        (is (= 590M (:account/quantity ira))
            "Shares have been transfered into IRA")))))

(def ^:private sched-context
  (conj base-context
        #:image{:body (-> "resources/fixtures/scheduled_transactions.gnucash"
                          io/input-stream
                          read-bytes)
                :user "john@doe.com"
                :content-type "application/gnucash"
                :original-filename "scheduled_transactions.gnucash"}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["scheduled_transactions.gnucash"]}))

(deftest import-scheduled-transactions
  (with-context sched-context
    (let [imp (find-import "Personal")
          {:keys [entity updates]} (execute-import imp)]
      (is (= {:total 1 :completed 1}
             (:scheduled-transaction (last updates)))
          "The progress is updated for the scheduled transactions")
      (is (seq-of-maps-like?
            [#:scheduled-transaction{:entity (util/->model-ref entity)
                                     :description "Paycheck"
                                     :memo nil
                                     :start-date (t/local-date 2016 1 15)
                                     :end-date (t/local-date 2018 12 31)
                                     :enabled true
                                     :date-spec {:days [:friday]}
                                     :last-occurrence nil
                                     :interval-type :week
                                     :interval-count 2
                                     :items [#:scheduled-transaction-item{:action :debit
                                                                          :account (account-ref "Checking")
                                                                          :quantity 1000M
                                                                          :memo nil}
                                             #:scheduled-transaction-item{:action :credit
                                                                          :account (account-ref "Salary")
                                                                          :quantity 1000M
                                                                          :memo nil}]}]
            (models/select #:scheduled-transaction{:entity entity}))
          "The scheduled transactions are available after import."))))
