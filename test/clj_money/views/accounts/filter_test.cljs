(ns clj-money.views.accounts.filter-test
  (:require [cljs.test :refer [deftest testing is]]
            [cljs-time.core :as t]
            [clj-money.views.accounts.filter :as f]))

(def ^:private used [(t/local-date 2020 1 1) (t/local-date 2020 1 1)])

(def ^:private checking
  {:id 1
   :account/name "Checking"
   :account/user-tags #{:mandatory}
   :account/transaction-date-range used})

(def ^:private savings
  {:id 2
   :account/name "Savings"
   :account/user-tags #{:discretionary}})

(def ^:private old-checking
  {:id 3
   :account/name "Old Checking"
   :account/hidden true})

(def ^:private all-accounts [checking savings old-checking])

(defn- names
  ([options] (names options all-accounts))
  ([options accounts]
   (->> accounts
        (filter (f/account-filter-fn options accounts))
        (map :account/name))))

(deftest filter-accounts
  (testing "with no options"
    (is (= ["Checking" "Savings"]
           (names {}))
        "The hidden accounts are left out"))
  (testing "showing hidden accounts"
    (is (= ["Checking" "Savings" "Old Checking"]
           (names {:show-hidden? true}))))
  (testing "by tag"
    (is (= ["Savings"]
           (names {:tags #{"discretionary"}}))
        "The accounts with any of the tags are shown"))
  (testing "untagged"
    (is (= ["Old Checking"]
           (names {:tags #{"_untagged"}
                   :show-hidden? true}))
        "The accounts without tags are shown"))
  (testing "without transactions"
    (is (= ["Savings"]
           (names {:unused-only? true}))
        "The accounts that have never had a transaction are shown")
    (is (= ["Savings" "Old Checking"]
           (names {:unused-only? true
                   :show-hidden? true}))
        "Hidden accounts are still left out unless asked for")
    (is (= ["Expenses" "Food"]
           (names {:unused-only? true}
                  [{:id 10
                    :account/name "Expenses"}
                   {:id 11
                    :account/name "Food"
                    :account/parent-ids [10]}
                   {:id 12
                    :account/name "Rent"
                    :account/parent-ids [10]
                    :account/transaction-date-range used}]))
        "The ancestors of an account without transactions are shown, so it can be reached")))
