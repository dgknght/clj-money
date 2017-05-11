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
  (io/input-stream "resources/fixtures/sample.gnucash"))

(def ^:private accounts
  [{:name "Assets"
    :id "d005a139a1aaab6899867923509b96ca"
    :parent-id "cd97d4a64800f0277ab992749779108c"
    :type :asset}
   {:name "Checking"
    :id "ed92489659ab879fb9354a3a050fb65d"
    :parent-id "d005a139a1aaab6899867923509b96ca"
    :type :asset}
   {:name "Income"
    :id "dff5746dbbaf805f1a8ac3ceb5d1a234"
    :parent-id "cd97d4a64800f0277ab992749779108c"
    :type :income}
   {:name "Salary"
    :id "1b71fd298aeca1a18d35b04a7618e76e"
    :parent-id "dff5746dbbaf805f1a8ac3ceb5d1a234"
    :type :income}
   {:name "Expenses"
    :id "abff103816fb2e5cb93778b1ea51ca45"
    :parent-id "cd97d4a64800f0277ab992749779108c"
    :type :expense}
   {:name "Groceries"
    :id "835bfe9b2728976d06e63b90aea8c090"
    :parent-id "abff103816fb2e5cb93778b1ea51ca45"
    :type :expense}
   {:name "Liabilities"
    :id "9ee0484c7788656a0800e28ec8cefaff"
    :parent-id "cd97d4a64800f0277ab992749779108c"
    :type :liability}
   {:name "Credit Card"
    :id "337685830c05f47b2b09734a05a7c1a2"
    :parent-id "9ee0484c7788656a0800e28ec8cefaff"
    :type :liability}])

(deftest read-gnucash-source
  (let [accounts-found (atom [])]
    (read-source :gnucash input (->Callback (fn [a]
                                              (swap! accounts-found #(conj % a)))))
    (is (= accounts @accounts-found) "The correct accounts are found")))
