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
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.entities :as entities]
            [clj-money.reports :as reports]
            [clj-money.import :refer [read-source]]
            [clj-money.import.gnucash :as gnucash]))

(def ^:private input
  (io/input-stream "resources/fixtures/budget_sample.gnucash"))

(def ^:private declarations
  [{:record-type :commodity
    :record-count 1}
   {:record-type :account
    :record-count 9}
   {:record-type :transaction
    :record-count 6}
   {:record-type :budget
    :record-count 1}])

(def ^:private accounts
  [{:name "Checking"
    :id "ed92489659ab879fb9354a3a050fb65d"
    :parent-id "d005a139a1aaab6899867923509b96ca"
    :type :asset}
   {:name "Salary"
    :id "1b71fd298aeca1a18d35b04a7618e76e"
    :parent-id "dff5746dbbaf805f1a8ac3ceb5d1a234"
    :type :income}
   {:name "Groceries"
    :id "835bfe9b2728976d06e63b90aea8c090"
    :parent-id "abff103816fb2e5cb93778b1ea51ca45"
    :type :expense}
   {:name "Credit Card"
    :id "337685830c05f47b2b09734a05a7c1a2"
    :parent-id "9ee0484c7788656a0800e28ec8cefaff"
    :type :liability}])

(def ^:private transactions
  [{:id "3ab51576141406703644c0a27579c057"
    :transaction-date (t/local-date 2015 1 1)
    :description "Paycheck"
    :items [{:action :debit
             :account-id "ed92489659ab879fb9354a3a050fb65d"
             :amount 1000M
             :reconciled false}
            {:action :credit
             :account-id "1b71fd298aeca1a18d35b04a7618e76e"
             :amount 1000M
             :reconciled false}]}
   {:id "cfade80943455ccc30a80c51df065981"
    :transaction-date (t/local-date 2015 1 4)
    :description "Kroger"
    :items [{:action :debit
             :account-id "835bfe9b2728976d06e63b90aea8c090"
             :amount 100M
             :reconciled false}
            {:action :credit
             :account-id "ed92489659ab879fb9354a3a050fb65d"
             :amount 100M
             :reconciled false}]}
   {:id "0cb1dcd9a02998ad09b79909d56eb8d6"
    :transaction-date (t/local-date 2015 1 11)
    :description "Kroger"
    :items [{:action :debit
             :account-id "835bfe9b2728976d06e63b90aea8c090"
             :amount 100M
             :reconciled false}
            {:action :credit
             :account-id "ed92489659ab879fb9354a3a050fb65d"
             :amount 100M
             :reconciled false}]}
   {:id "58072b476715968e046a344fe37c4141"
    :transaction-date (t/local-date 2015 1 12)
    :description "Kroger"
    :items [{:action :debit
             :account-id "ed92489659ab879fb9354a3a050fb65d"
             :amount 10M
             :reconciled false}
            {:action :credit
             :account-id "835bfe9b2728976d06e63b90aea8c090"
             :amount 10M
             :reconciled false}]}
   {:id "a253d6afada480928f2ac3c1777eb5f3"
    :transaction-date (t/local-date 2015 1 15)
    :description "Paycheck"
    :items [{:action :debit
             :account-id "ed92489659ab879fb9354a3a050fb65d"
             :amount 1000M
             :reconciled false}
            {:action :credit
             :account-id "1b71fd298aeca1a18d35b04a7618e76e"
             :amount 1000M
             :reconciled false}]}
   {:id "17161f9669b86317f9d381589b847190"
    :transaction-date (t/local-date 2015 1 18)
    :description "Kroger"
    :items [{:action :debit
             :account-id "835bfe9b2728976d06e63b90aea8c090"
             :amount 100M
             :reconciled false}
            {:action :credit
             :account-id "337685830c05f47b2b09734a05a7c1a2"
             :amount 100M
             :reconciled false}]}])

(def ^:private budgets
  [{:id "cdb9ef21d19570acd3cf6a23d1a97ce8"
    :name "2017"
    :start-date (t/local-date 2017 1 1)
    :period :month
    :period-count 12
    :items [{:account-id "1b71fd298aeca1a18d35b04a7618e76e"
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
            {:account-id "835bfe9b2728976d06e63b90aea8c090"
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
  [store record record-type]
  (swap! store (fn [f]
                 (cond-> f
                   record
                   (update-in [record-type]
                              #((fnil conj []) % record))))))

(deftest read-gnucash-source
  (let [found (atom {})]
    (read-source :gnucash
                 input
                 (partial track-record found))
    (is (= declarations (:declaration @found)) "The correct declarations are found")
    (is (= accounts (:account @found)) "The correct accounts are found")
    (is (= budgets (:budget @found)) "The current budgets are found")
    (is (= transactions (:transaction @found)) "The correct transactions are found")))

(def ^:private commodities-input
  (io/input-stream "resources/fixtures/sample_with_commodities.gnucash"))

(def ^:private commodities
  [{:name "Apple, Inc."
    :symbol "AAPL"
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
     :record-count 2}
    {:record-type :price
     :record-count 2}
    {:record-type :account
     :record-count 11}
    {:record-type :transaction
     :record-count 8}})

(deftest read-gnucash-source-with-commodities
  (let [found (atom {})]
    (read-source :gnucash
                 commodities-input
                 (partial track-record found))
    (is (= commodities (:commodity @found)) "The correct commodities are found")
    (is (= prices (:price @found)) "The correct prices are found")
    (is (= commodity-declarations (set (:declaration @found)))
        "The correct declarations are found")))
