(ns clj-money.import.gnucash-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff]]
            [clj-money.models.entities :as entities]
            [clj-money.reports :as reports]
            [clj-money.import :refer [read-source]]
            [clj-money.import.gnucash :as gnucash]))

(def ^:private input
  (io/input-stream "resources/fixtures/budget_sample.gnucash"))

(def ids
  {:checking    "ed92489659ab879fb9354a3a050fb65d"
   :salary      "1b71fd298aeca1a18d35b04a7618e76e"
   :groceries   "835bfe9b2728976d06e63b90aea8c090"
   :credit-card "337685830c05f47b2b09734a05a7c1a2"
   :401k        "fc053b4fc6b94898a5d6fa53ed203bd0"
   :apple-401k  "77bfb9a7eb53ebfd5dd13b22476f58dd"
   :ira         "294fd407b010e07e8fb54a4cab3fbd8c"
   :apple-ira   "d65092fee5cfdacab77e7b37942675bb"
   :other-inc   "8c62d43a4feee76da698912d7ff3a6ca"})

(def ^:private declarations
  [{:record-type :commodity
    :record-count 2}
   {:record-type :account
    :record-count 9}
   {:record-type :transaction
    :record-count 6}
   {:record-type :budget
    :record-count 1}])

(def ^:private accounts
  [{:name "Checking"
    :id (:checking ids)
    :parent-id "d005a139a1aaab6899867923509b96ca"
    :type :asset
    :commodity {:exchange :iso4217
                :symbol "USD"}}
   {:name "Salary"
    :id (:salary ids)
    :parent-id "dff5746dbbaf805f1a8ac3ceb5d1a234"
    :type :income
    :commodity {:exchange :iso4217
                :symbol "USD"}}
   {:name "Groceries"
    :id (:groceries ids)
    :parent-id "abff103816fb2e5cb93778b1ea51ca45"
    :type :expense
    :commodity {:exchange :iso4217
                :symbol "USD"}}
   {:name "Credit Card"
    :id (:credit-card ids)
    :parent-id "9ee0484c7788656a0800e28ec8cefaff"
    :type :liability
    :commodity {:exchange :iso4217
                :symbol "USD"}}])

(def ^:private transactions
  [{:transaction-date (t/local-date 2015 1 1)
    :description "Paycheck"
    :items [{:action :debit
             :account-id (:checking ids)
             :amount 1000M
             :reconciled false}
            {:action :credit
             :account-id (:salary ids)
             :amount 1000M
             :reconciled false}]}
   {:transaction-date (t/local-date 2015 1 4)
    :description "Kroger"
    :items [{:action :debit
             :account-id (:groceries ids)
             :amount 100M
             :reconciled false}
            {:action :credit
             :account-id (:checking ids)
             :amount 100M
             :reconciled false}]}
   {:transaction-date (t/local-date 2015 1 11)
    :description "Kroger"
    :items [{:action :debit
             :account-id (:groceries ids)
             :amount 100M
             :reconciled false}
            {:action :credit
             :account-id (:checking ids)
             :amount 100M
             :reconciled false}]}
   {:transaction-date (t/local-date 2015 1 12)
    :description "Kroger"
    :items [{:action :debit
             :account-id (:checking ids)
             :amount 10M
             :reconciled false}
            {:action :credit
             :account-id (:groceries ids)
             :amount 10M
             :reconciled false}]}
   {:transaction-date (t/local-date 2015 1 15)
    :description "Paycheck"
    :items [{:action :debit
             :account-id (:checking ids)
             :amount 1000M
             :reconciled false}
            {:action :credit
             :account-id (:salary ids)
             :amount 1000M
             :reconciled false}]}
   {:transaction-date (t/local-date 2015 1 18)
    :description "Kroger"
    :items [{:action :debit
             :account-id (:groceries ids)
             :amount 100M
             :reconciled false}
            {:action :credit
             :account-id (:credit-card ids)
             :amount 100M
             :reconciled false}]}])

(def ^:private budgets
  [{:id "cdb9ef21d19570acd3cf6a23d1a97ce8"
    :name "2017"
    :start-date (t/local-date 2017 1 1)
    :period :month
    :period-count 12
    :items [{:account-id (:salary ids)
             :periods #{{:index 0
                         :amount 1000M}
                        {:index 1
                         :amount 1000M}
                        {:index 2
                         :amount 1000M}
                        {:index 3
                         :amount 1000M}
                        {:index 4
                         :amount 1000M}
                        {:index 5
                         :amount 1000M}
                        {:index 6
                         :amount 1000M}
                        {:index 7
                         :amount 1000M}
                        {:index 8
                         :amount 1000M}
                        {:index 9
                         :amount 1000M}
                        {:index 10
                         :amount 1000M}
                        {:index 11
                         :amount 1000M}}}
            {:account-id (:groceries ids)
             :periods #{{:index 0
                         :amount 200M}
                        {:index 1
                         :amount 200M}
                        {:index 2
                         :amount 250M}
                        {:index 3
                         :amount 250M}
                        {:index 4
                         :amount 275M}
                        {:index 5
                         :amount 275M}
                        {:index 6
                         :amount 200M}
                        {:index 7
                         :amount 200M}
                        {:index 8
                         :amount 250M}
                        {:index 9
                         :amount 250M}
                        {:index 10
                         :amount 275M}
                        {:index 11
                         :amount 275M}}}]}])

(defn- track-record
  [store record]
  (let [{:keys [record-type ignore?]} (meta record)]
    (if ignore?
      store
      (update-in store
                 [record-type]
                 #((fnil conj []) % record)))))

(deftest read-gnucash-source
  (let [found (reduce track-record {} (read-source :gnucash input))]
    (is (= declarations (:declaration found)) "The correct declarations are found")
    (pprint-diff accounts (:account found))
    (is (= accounts (:account found)) "The correct accounts are found")
    (pprint-diff budgets (:budget found))
    (is (= budgets (:budget found)) "The current budgets are found")
    (pprint-diff transactions (:transaction found))
    (is (= transactions (:transaction found)) "The correct transactions are found")))

(def ^:private commodities-input
  (io/input-stream "resources/fixtures/sample_with_commodities.gnucash"))

(def ^:private commodities
  [{:name "USD"
    :symbol "USD"
    :type :currency}
   {:name "Apple, Inc."
    :symbol "AAPL"
    :type :stock
    :exchange :nasdaq}])

(def ^:private prices
  [{:trade-date (t/local-date 2015 1 30)
    :price 12.00M
    :exchange :nasdaq
    :symbol "AAPL"}
   {:trade-date (t/local-date 2015 1 17)
    :price 10.00M
    :exchange :nasdaq
    :symbol "AAPL"}])

(def ^:private commodity-declarations
  #{{:record-type :commodity
     :record-count 3}
    {:record-type :price
     :record-count 2}
    {:record-type :account
     :record-count 11}
    {:record-type :transaction
     :record-count 8}})

(def ^:private accounts-with-commodities
  (-> accounts
      (concat [{:id (:401k ids)
                :parent-id "d005a139a1aaab6899867923509b96ca"
                :name "401k"
                :type :asset
                :commodity {:exchange :iso4217
                            :symbol "USD"}}
               {:id (:apple-401k ids)
                :parent-id (:401k ids)
                :name "Apple, Inc"
                :type :asset
                :commodity {:exchange :nasdaq
                            :symbol "AAPL"}}])
      set))

(deftest read-gnucash-source-with-commodities
  (let [found (reduce track-record {} (read-source :gnucash commodities-input))]
    (pprint-diff commodities (:commodity found))
    (is (= commodities (:commodity found)) "The correct commodities are found")
    (pprint-diff prices (:price found))
    (is (= prices (:price found)) "The correct prices are found")
    (pprint-diff commodity-declarations (set (:declaration found)))
    (is (= commodity-declarations (set (:declaration found)))
        "The correct declarations are found")
    (pprint-diff accounts-with-commodities (set (:account found)))
    (is (= accounts-with-commodities
           (set (:account found)))
        "The correct accounts are found")))

(def ^:private extended-commodities-input
  (io/input-stream "resources/fixtures/sample_with_commodities_ext.gnucash"))

(deftest read-gnucash-source-with-trading-actions
  (let [found (reduce track-record {} (read-source
                                        :gnucash
                                        extended-commodities-input))
        expected-transactions [{:transaction-date (t/local-date 2015 1 16)
                                :description "Retirement"
                                :items [{:action :debit
                                         :account-id (:401k ids)
                                         :amount 1000M
                                         :reconciled false}
                                        {:action :credit
                                         :account-id (:checking ids)
                                         :amount 1000M
                                         :reconciled false}]}
                               {:transaction-date (t/local-date 2015 1 17)
                                :description "Purchase shares AAPL"
                                :action :buy
                                :symbol "AAPL"
                                :exchange :nasdaq
                                :shares 100M
                                :account-id (:401k ids)
                                :commodity-account-id (:apple-401k ids)
                                :items [{:action :debit
                                         :account-id (:apple-401k ids)
                                         :amount 1000M
                                         :reconciled false}
                                        {:action :credit
                                         :account-id (:401k ids)
                                         :amount 1000M
                                         :reconciled false}]}
                               {:transaction-date (t/local-date 2015 3 2)
                                :description "Transfer shares of AAPL"
                                :action :transfer
                                :shares 100M
                                :value 1000M
                                :from-account-id (:apple-401k ids)
                                :to-account-id (:apple-ira ids)
                                :items [{:action :debit
                                         :account-id (:apple-ira ids)
                                         :amount 1000M
                                         :reconciled false}
                                        {:action :credit
                                         :account-id (:apple-401k ids)
                                         :amount 1000M
                                         :reconciled false}]}
                               {:transaction-date (t/local-date 2015 4 1)
                                :description "Stock Split"
                                :action :split
                                :shares-gained 100M
                                :commodity-account-id (:apple-ira ids)
                                :items [{:action :debit
                                         :account-id (:apple-ira ids)
                                         ; TODO add some shares here
                                         :amount 0M
                                         :reconciled false}
                                        {:action :debit
                                         :account-id (:ira ids)
                                         :amount 1M
                                         :reconciled false}
                                        {:action :credit
                                         :account-id (:other-inc ids)
                                         :amount 1M
                                         :reconciled false}]}]
        inv-account-ids (->> [:ira :apple-ira :401k :apple-401k]
                             (map #(% ids))
                             set)
        actual-transactions (filter (fn [transaction]
                                      (some #(inv-account-ids (:account-id %))
                                            (:items transaction)))
                                    (:transaction found))]
    (pprint-diff expected-transactions actual-transactions)
    (is (= expected-transactions actual-transactions)
        "The correct transactions are found")))
