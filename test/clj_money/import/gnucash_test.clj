(ns clj-money.import.gnucash-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.core.async :as a]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [uuid]]
            [clj-money.factories.user-factory]
            [clj-money.import :refer [read-source]]
            [clj-money.import.gnucash]))

(defn- track-record
  [store {:as record :import/keys [ignore? record-type]}]
  (if ignore?
    store
    (update-in store
               [record-type]
               #((fnil conj []) % record))))

(defn- execute-import
  [input-path]
  (with-redefs [uuid (constantly "00000000000000000000000000000001")]
    (a/<!! (a/reduce
             track-record
             {}
             (read-source :gnucash
                          [(io/input-stream input-path)])))))

(defn- execute-test
  [spec-path]
  (let [{:keys [input expectations]} (-> spec-path slurp read-string)
        result (execute-import input)]
    (doall (for [[k expected] expectations
                 :let [actual (get-in result [k])]]
             (is (= expected actual) (str "Incorrect results for " k))))))

(deftest ^:multi-threaded read-gnucash-source-with-budget
  (execute-test "resources/fixtures/gnucash_test/budget.edn"))

(deftest ^:multi-threaded read-gnucash-source-with-commodities
  (execute-test "resources/fixtures/gnucash_test/commodities.edn"))

(deftest ^:multi-threaded read-gnucash-source-with-trading-actions
  (execute-test "resources/fixtures/gnucash_test/commodities_ext.edn"))

(deftest ^:multi-threaded read-gnucash-source-with-scheduled-transactions
  (execute-test "resources/fixtures/gnucash_test/scheduled_transactions.edn"))
