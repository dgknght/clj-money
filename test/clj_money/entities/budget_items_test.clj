(ns clj-money.entities.budget-items-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is]]
            [clojure.core.async :as a]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.entities :as entities]
            [clj-money.entity-helpers :as helpers :refer [assert-invalid
                                                         assert-updated]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-account
                                            find-budget]]
            [clj-money.test-helpers :refer [dbtest]]))

(defn- attributes []
  #:budget-item{:account (util/->entity-ref (find-account "Groceries")) 
                :periods [101M 102M 103M]})

(def ^:private ctx
  (conj basic-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :period [3 :month]
                 :start-date (t/local-date 2016 1 1)}))

(dbtest create-a-budget-item
  (with-context ctx
    (let [budget (find-budget "2016")]
      (assert-updated budget
                      {:budget/items [(attributes)]}))))

(dbtest account-is-required
  (with-context ctx
    (assert-invalid
      (assoc (find-budget "2016")
             :budget/items
             (dissoc (attributes) :budget-item/account))
      {:budget/items {0 ["Value is invalid"]}})))

(dbtest account-must-belong-to-budget-entity
  (with-context ctx
    (assert-invalid
      (assoc (find-budget "2016")
             :budget/items
             [(assoc (attributes)
                    :budget-item/account (find-account "Sales"))])
      {:budget/items
       ["Account must belong to the same entity as the budget"]})))

(dbtest period-count-must-match-the-budget
  (with-context ctx
    (assert-invalid
      (assoc (find-budget "2016")
             :budget/items
             [(assoc (attributes)
                    :budget-item/periods [101M])])
      {:budget-item/periods ["Must have the number of periods specified by the budget"]})))

(dbtest a-total-based-spec-can-be-specified
  (with-context ctx
    (assert-updated
      (find-budget "2016")
      {:budget/items [(assoc (attributes)
                             :budget-item/spec {:total 1200M})]})))

(dbtest an-average-based-spec-can-be-specified
  (with-context ctx
    (assert-updated
      (find-budget "2016")
      {:budget/items [(assoc (attributes)
                             :budget-item/spec {:average 120M})]})))

(dbtest a-week-based-spec-can-be-specified
  (with-context ctx
    (assert-updated
      (find-budget "2016")
      {:budget/items [(assoc (attributes)
                             :budget-item/spec {:start-date (t/local-date 2016 1 2)
                                                :amount 100M
                                                :week-count 2})]})))

(dbtest a-historical-spec-can-be-specified
  (with-context ctx
    (assert-updated
      (find-budget "2016")
      {:budget/items [(assoc (attributes)
                           :budget-item/spec {:start-date (t/local-date 2016 1 1)
                                              :round-to 2})]})))

(def ^:private existing-context
  (conj basic-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :period [3 :month]
                 :start-date (t/local-date 2016 1 1)
                 :items [#:budget-item{:account "Groceries"
                                       :periods [101M 102M 103M]
                                       :spec {:total 1000M}}]}))

(dbtest update-a-budget-item
  (with-context existing-context
    (let [{:as budget :budget/keys [items]} (find-budget "2016")]
      (assert-updated budget
                      {:budget/items (assoc-in items
                                               [0 :budget-item/periods]
                                               [111M 222M 333M])}))))

(dbtest update-a-budget-item-spec
  (with-context existing-context
    (let [{:as budget :budget/keys [items]} (find-budget "2016")]
      (assert-updated budget
                      {:budget/items (assoc-in items
                                               [0 :budget-item/spec]
                                               {:average 100M})}))))

(dbtest remove-an-item
  (with-context existing-context
    (let [out-chan (a/chan)
          budget (find-budget "2016")
          result (entities/put (assoc budget :budget/items [])
                             :out-chan out-chan)]
      (is (nil? (seq (:budget/items result)))
          "The item is removed from the return value")
      (is (nil? (seq (:budget/items (entities/find budget))))
          "The item is removed from the retrieved value")
      (let [[[before after] _ch] (a/alts!! [out-chan (a/timeout 1000)])]
        (is (= budget before)
            "The before value is passed to the output channel")
        (is (= result after)
            "The after value is passed to the output channel")))))
