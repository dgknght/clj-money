(ns clj-money.db.datomic.queries-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.pprint :refer [pprint]]
            [datomic.api :as d]))

(defn- recursion-rule
  [rel-key target-key upward?]
  [['(match-and-recurse ?e ?v)
    ['?e target-key '?v]]
   ['(match-and-recurse ?e1 ?v)
    [(if upward? '?e2 '?e1) rel-key (if upward? '?e1 '?e2)]
    '(match-and-recurse ?e2 ?v)]])

(deftest query-recursively
  (let [accounts [[1 :account/name "Checking"]
                  [2 :account/name "Savings"]
                  [3 :account/name "Car"]
                  [3 :account/parent 2]
                  [4 :account/name "Doug"]
                  [4 :account/parent 3]
                  [5 :account/name "Eli"]
                  [5 :account/parent 3]
                  [6 :account/name "Reserve"]
                  [6 :account/parent 2]]]
    (testing "infinite levels downward"
      (is (= #{"Doug" "Eli" "Car" "Reserve" "Savings"}
             (set
               (map first
                    (d/q '[:find ?name
                           :in $ % ?input
                           :where [?a :account/name ?name]
                           (match-and-recurse ?a ?input)]
                         accounts
                         (recursion-rule :account/parent :account/name false)
                         "Savings"))))))
    (testing "infinite levels upward"
      (is (= #{"Doug" "Car" "Savings"}
             (set
               (map first
                    (d/q '[:find ?name
                           :in $ % ?input
                           :where [?a :account/name ?name]
                           (match-and-recurse ?a ?input)]
                         accounts
                         (recursion-rule :account/parent :account/name true)
                         "Doug"))))))))
