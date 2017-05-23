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
            [clj-money.import :refer [read-source ->Callback]]
            [clj-money.import.gnucash :as gnucash]))

(def ^:private input
  (io/input-stream "resources/fixtures/budget_sample.gnucash"))

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
                         :amount 250M}
                        {:index 2
                         :amount 250M}
                        {:index 3
                         :amount 275M}
                        {:index 4
                         :amount 275M}
                        {:index 5
                         :amount 200M}
                        {:index 6
                         :amount 200M}
                        {:index 7
                         :amount 250M}
                        {:index 8
                         :amount 250M}
                        {:index 9
                         :amount 275M}
                        {:index 10
                         :amount 275M}
                        {:index 11
                         :amount 275M}}}]}])

(deftest read-gnucash-source
  (let [accounts-found (atom [])
        transactions-found (atom [])
        budgets-found (atom [])]
    (read-source :gnucash input (->Callback (fn [a]
                                              (swap! accounts-found #(conj % a)))
                                            (fn [b]
                                              (swap! budgets-found #(conj % b)))
                                            (fn [t]
                                              (swap! transactions-found #(conj % t)))))
    (is (= accounts @accounts-found) "The correct accounts are found")

    (pprint {:expected budgets
             :actual @budgets-found
             :diff (diff budgets @budgets-found)})

    (is (= budgets @budgets-found) "The current budgets are found")
    (is (= transactions @transactions-found) "The correct transactions are found")))
