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

(def accounts
  [[1 :account/name "Checking"]
   [2 :account/name "Savings"]
   [3 :account/name "Car"]
   [3 :account/parent 2]
   [4 :account/name "Doug"]
   [4 :account/parent 3]
   [5 :account/name "Eli"]
   [5 :account/parent 3]
   [6 :account/name "Reserve"]
   [6 :account/parent 2]])

(deftest query-recursively
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
                       "Doug")))))))

(deftest query-with-or
  (testing "query by entity"
    (is (= #{"Checking" "Reserve"}
           (set
             (map first
                  (d/q '[:find ?name
                         :in $ ?ids
                         :where [?a :account/name ?name]
                                [(contains? ?ids ?a)]]
                       accounts
                       #{1 6}))))))
  (testing "query by attribute"
    (is (= #{"Car" "Reserve" "Doug" "Eli"}
           (set
             (map first
                  (d/q '[:find ?name
                         :in $ ?parent
                         :where [?a :account/parent ?account-parent]
                                [?a :account/name ?name]
                                [(contains? ?parent ?account-parent)]]
                       accounts
                       #{2 3})))))))
