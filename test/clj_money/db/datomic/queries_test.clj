(ns clj-money.db.datomic.queries-test
  (:require [clojure.test :refer [deftest testing is]]
            [datomic.api :as d]))


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
                           (match-and-recurse ?a :account/parent :account/name ?input)]
                         accounts
                         '[[(match-and-recurse ?e ?a-rel ?a-target ?v)
                            [?e ?a-target ?v]]
                           [(match-and-recurse ?e1 ?a-rel ?a-target ?v)
                            [?e1 ?a-rel ?e2]
                            (match-and-recurse ?e2 ?a-rel ?a-target ?v)]]
                         "Savings"))))))
    (testing "infinite levels upward"
      (is (= #{"Doug" "Car" "Savings"}
             (set
               (map first
                    (d/q '[:find ?name
                           :in $ % ?input
                           :where [?a :account/name ?name]
                           (match-and-recurse ?a :account/parent :account/name ?input)]
                         accounts
                         '[[(match-and-recurse ?e ?a-rel ?a-target ?v)
                            [?e ?a-target ?v]]
                           [(match-and-recurse ?e1 ?a-rel ?a-target ?v)
                            [?e2 ?a-rel ?e1]
                            (match-and-recurse ?e2 ?a-rel ?a-target ?v)]]
                         "Doug"))))))))
