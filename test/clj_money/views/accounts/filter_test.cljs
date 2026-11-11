(ns clj-money.views.accounts.filter-test
  (:require [cljs.test :refer [deftest testing is]]
            [clj-money.views.accounts.filter :as f]))

(def ^:private checking
  #:account{:name "Checking"
            :user-tags #{:mandatory}})

(def ^:private savings
  #:account{:name "Savings"
            :user-tags #{:discretionary}})

(def ^:private old-checking
  #:account{:name "Old Checking"
            :hidden true})

(def ^:private all-accounts [checking savings old-checking])

(defn- names
  [options]
  (->> all-accounts
       (filter (f/account-filter-fn options))
       (map :account/name)))

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
        "The accounts without tags are shown")))
