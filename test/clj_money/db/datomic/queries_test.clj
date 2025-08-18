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
    (testing "filter by non-id attribute"
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
    (testing "filter by id"
      (is (= #{"Doug" "Eli" "Car" "Reserve" "Savings"}
             (set
               (map first
                    (d/q '[:find ?name
                           :in $ % ?a
                           :where [?x :account/name ?name]
                           (match-and-recurse ?x ?a)]
                         accounts
                         '[[(match-and-recurse ?e ?target)
                            [(= ?e ?target)]]
                           [(match-and-recurse ?e ?target)
                            [?e :account/parent ?parent]
                            (match-and-recurse ?parent ?target)]] 
                         2)))))))
  (testing "infinite levels upward"
    (testing "filter by non-id attribute"
      (is (= #{"Doug" "Car" "Savings"}
             (set
               (map first
                    (d/q '[:find ?name
                           :in $ % ?input
                           :where [?a :account/name ?name]
                           (match-and-recurse ?a ?input)]
                         accounts
                         (recursion-rule :account/parent :account/name true)
                         "Doug"))))))
    (testing "filter by id"
      (is (= #{"Doug" "Car" "Savings"}
             (set
               (map first
                    (d/q '[:find ?name
                           :in $ % ?a
                           :where [?x :account/name ?name]
                           (match-and-recurse ?x ?a)]
                         accounts
                         '[[(match-and-recurse ?e ?target)
                            [(= ?e ?target)]]
                           [(match-and-recurse ?e ?target)
                            [?child :account/parent ?e]
                            (match-and-recurse ?child ?target)]] 
                         4))))))))

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

(def ^:private transactions
  (conj accounts
        [7 :account/name "Groceries"]
        [8 :account/name "Rent"]
        [9 :account/name "Salary"]
        [21 :transaction-item/action :debit]
        [21 :transaction-item/account 1]
        [21 :transaction-item/quantity 1000M]
        [22 :transaction-item/action :credit]
        [22 :transaction-item/account 9]
        [22 :transaction-item/quantity 1000M]
        [11 :transaction/transaction-date "2020-01-01"]
        [11 :transaction/description "The Man"]
        [11 :transaction/items 21]
        [11 :transaction/items 22]
        [23 :transaction-item/action :credit]
        [23 :transaction-item/account 1]
        [23 :transaction-item/quantity 100M]
        [24 :transaction-item/action :debit]
        [24 :transaction-item/account 7]
        [24 :transaction-item/quantity 100M]
        [12 :transaction/transaction-date "2020-01-02"]
        [12 :transaction/description "Kroger"]
        [12 :transaction/items 23]
        [12 :transaction/items 24]))

(deftest join-an-item-to-a-transaction
  (is (= #{["2020-01-01" 1000M]
           ["2020-01-02" 100M]}
         (set
           (d/q '[:find ?date ?quantity
                  :in $ ?account
                  :where [?i :transaction-item/account ?account]
                         [?i :transaction-item/quantity ?quantity]
                         [?t :transaction/items ?i]
                         [?t :transaction/transaction-date ?date]]
                transactions
                1)))))

(def ^:private accounts-with-expenses
  (conj accounts
        [7 :account/name "Rent"]
        [7 :account/tags :mandatory]
        [8 :account/name "Groceries"]
        [8 :account/tags :mandatory]
        [8 :account/tags :food]
        [9 :account/name "Dining"]
        [9 :account/tags :discretionary]
        [9 :account/tags :food]
        [10 :account/name "FIT"]
        [10 :account/tags :tax]
        [11 :account/name "Social Security"]
        [11 :account/tags :tax]))

(deftest query-account-tags
  (testing "query against a single tag"
    (is (= #{"Rent" "Groceries"}
           (->> (d/q '[:find ?name
                       :in $ ?tag
                       :where [?a :account/name ?name]
                              [?a :account/tags ?tag]]
                     accounts-with-expenses
                     :mandatory)
                (map first)
                set))))
  (testing "query against multiple tags"
    (is (= #{"Rent" "Groceries" "Dining"}
           (->> (d/q '[:find ?name
                       :in $ ?t
                       :where [?a :account/name ?name]
                              [?a :account/tags ?tag]
                              [(contains? ?t ?tag)]]
                     accounts-with-expenses
                     #{:mandatory :discretionary})
                (map first)
                set)))))

(def ^:private recon-data
  [[101 :account/name "Checking"]
   [102 :account/name "Salary"]
   [103 :account/name "Rent"]

   ; Paycheck
   [201 :transaction-item/action :debit]
   [201 :transaction-item/account 101]
   [201 :transaction-item/quantity 1000M]
   [202 :transaction-item/action :credit]
   [202 :transaction-item/account 102]
   [202 :transaction-item/quantity 1000M]
   [301 :transaction/transaction-date "2020-01-01"]
   [301 :transaction/description "Paycheck"]
   [301 :transaction/items 201]
   [301 :transaction/items 202]

   ; Rent
   [203 :transaction-item/action :debit]
   [203 :transaction-item/account 103]
   [203 :transaction-item/quantity 500M]
   [204 :transaction-item/action :credit]
   [204 :transaction-item/account 101]
   [204 :transaction-item/quantity 500M]
   [302 :transaction/transaction-date "2020-01-02"]
   [302 :transaction/description "Kroger"]
   [302 :transaction/items 203]
   [302 :transaction/items 204]

   ; Reconciliations
   [401 :reconciliation/end-of-period "2020-01-31"]
   [401 :reconciliation/balance 500M]
   [401 :reconciliation/account 101]
   [401 :reconciliation/items 201]
   [401 :reconciliation/items 204]])

(deftest query-transactions-for-reconciliation
  (is (= #{["2020-01-01" :debit 1000M]
           ["2020-01-02" :credit 500M]}
         (set (d/q '[:find ?date ?action ?quantity
                     :where [?recon :reconciliation/items ?item]
                            [?item :transaction-item/action ?action]
                            [?item :transaction-item/quantity ?quantity]
                            [?transaction :transaction/items ?item]
                            [?transaction :transaction/transaction-date ?date]
                     :in $ ?recon]
                   recon-data
                   401)))))

(defn- init-db
  [schema data]
  (let [uri "datomic:mem://test"
        _ (d/create-database uri)
        conn (d/connect uri)]
    @(d/transact conn schema)
    @(d/transact conn data)
    conn))

(def ^:private sched-trx-schema
  [{:db/ident :scheduled-transaction/enabled
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident :scheduled-transaction/id
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :scheduled-transaction/start-date
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :scheduled-transaction/end-date
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(def ^:private sched-trx-data
  [{:scheduled-transaction/enabled true
    :scheduled-transaction/id :always}
   {:scheduled-transaction/enabled false
    :scheduled-transaction/id :never}
   {:scheduled-transaction/enabled true
    :scheduled-transaction/id :sometimes
    :scheduled-transaction/start-date "2020-03-01"
    :scheduled-transaction/end-date "2020-03-31"}])

(deftest query-active-scheduled-transactions
  (let [conn (init-db sched-trx-schema sched-trx-data)
        qry '[:find ?id
              :in $ ?enabled ?as-of
              :where [?trx :scheduled-transaction/enabled ?enabled]
                     [?trx :scheduled-transaction/id ?id]
                     [(get-else $ ?trx :scheduled-transaction/start-date "0000-00-00") ?start-date]
                     [(<= ?start-date ?as-of)]
                     [(get-else $ ?trx :scheduled-transaction/end-date "9999-99-99") ?end-date]
                     [(<= ?as-of ?end-date)]]]
    (is (= #{:always}
           (->> (d/q qry (d/db conn) true "2020-02-28")
                (map first)
                set))
        "Transactions starting after the specified date are excluded")
    (is (= #{:always :sometimes}
           (->> (d/q qry (d/db conn) true "2020-03-01")
                (map first)
                set))
        "Transactions starting before and ending after the specified date are included")
    (is (= #{:always}
           (->> (d/q qry (d/db conn) true "2020-04-01")
                (map first)
                set))
        "Transactions ending before the specified date are excluded")))
