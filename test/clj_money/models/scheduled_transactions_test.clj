(ns clj-money.models.scheduled-transactions-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.models :as models]
            [clj-money.model-helpers :refer [assert-created
                                             assert-invalid
                                             assert-deleted]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account
                                            find-scheduled-transaction
                                            basic-context]]
            [clj-money.models.scheduled-transactions :as sched-trans]
            [clj-money.models.transactions :as trans]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:scheduled-transaction{:entity (find-entity "Personal")
                          :start-date (t/local-date 2021 3 31)
                          :end-date (t/local-date 2022 12 31)
                          :enabled true
                          :date-spec {:day :last}
                          :interval-type :month
                          :interval-count 1
                          :description "Paycheck"
                          :memo "scheduled"
                          :items [#:scheduled-transaction-item{:action :debit
                                                               :account (find-account "Checking")
                                                               :quantity 1000M}
                                  #:scheduled-transaction-item{:action :credit
                                                               :account (find-account "Salary")
                                                               :quantity 1000M}]})

(deftest create-a-scheduled-transaction
  (with-context
    (assert-created (attributes) :refs [:scheduled-transaction/entity])))

; (deftest entity-id-is-required
;   (binding [*attr* (dissoc (init-attr) :entity-id)]
;     (failed-validation [:entity-id] "Entity is required" (create-scheduled-transaction))))
; 
; (deftest description-is-required
;   (binding [*attr* (dissoc (init-attr) :description)]
;     (failed-validation [:description] "Description is required" (create-scheduled-transaction))))
; 
; (deftest interval-type-is-required
;   (binding [*attr* (dissoc (init-attr) :interval-type)]
;     (failed-validation [:interval-type] "Interval type is required" (create-scheduled-transaction))))
; 
; (deftest interval-type-can-be-week
;   (binding [*attr* (assoc (init-attr) :interval-type :week)]
;     (successful-creation (create-scheduled-transaction))))
; 
; (deftest interval-type-can-be-month
;   (binding [*attr* (assoc (init-attr) :interval-type :month)]
;     (successful-creation (create-scheduled-transaction))))
; 
; (deftest interval-type-can-be-year
;   (binding [*attr* (assoc (init-attr) :interval-type :year)]
;     (successful-creation (create-scheduled-transaction))))
; 
; (deftest interval-type-cannot-be-off-list
;   (binding [*attr* (assoc (init-attr) :interval-type :not-valid)]
;     (failed-validation [:interval-type] "Interval type must be day, week, month, or year" (create-scheduled-transaction))))
; 
; (deftest start-date-is-required
;   (binding [*attr* (dissoc (init-attr) :start-date)]
;     (failed-validation [:start-date] "Start date is required" (create-scheduled-transaction))))
; 
; (deftest date-spec-is-required
;   (binding [*attr* (dissoc (init-attr) :date-spec)]
;     (failed-validation [:date-spec] "Date spec is required" (create-scheduled-transaction))))
; 
; (deftest interval-count-is-required
;   (binding [*attr* (dissoc (init-attr) :interval-count)]
;     (failed-validation [:interval-count] "Interval count is required" (create-scheduled-transaction))))
; 
; (deftest interval-must-be-greater-than-zero
;   (binding [*attr* (assoc (init-attr) :interval-count 0)]
;     (failed-validation [:interval-count] "Interval count must be greater than zero" (create-scheduled-transaction))))
; 
; (deftest at-least-two-items-are-required
;   (binding [*attr* (update-in (init-attr) [:items] #(take 1 %))]
;     (failed-validation [:items]
;                        "There must be at least two items"
;                        (create-scheduled-transaction))))
; 
; (deftest sum-of-credits-must-equal-sum-of-debits
;   (binding [*attr* (assoc-in (init-attr) [:items 0 :quantity] 1001M)]
;     (failed-validation [:items]
;                        "The sum of debits must equal the sum of credits"
;                        (create-scheduled-transaction))))
; 
; (deftest item-account-id-is-required
;   (binding [*attr* (update-in (init-attr) [:items 0] dissoc :account-id)]
;     (failed-validation [:items 0 :account-id]
;                        "Account is required"
;                        (create-scheduled-transaction))))
; 
; (deftest item-action-is-required
;   (binding [*attr* (update-in (init-attr) [:items 0] dissoc :action)]
;     (failed-validation [:items 0 :action]
;                        "Action is required"
;                        (create-scheduled-transaction))))
; 
; (deftest item-action-must-be-credit-or-debit
;   (binding [*attr* (assoc-in (init-attr) [:items 0 :action] :not-valid)]
;     (failed-validation [:items 0 :action]
;                        "Action must be debit or credit"
;                        (create-scheduled-transaction))))
; 
; (deftest item-quantity-is-required
;   (binding [*attr* (update-in (init-attr) [:items 0] dissoc :quantity)]
;     (failed-validation [:items 0 :quantity]
;                        "Quantity is required"
;                        (create-scheduled-transaction))))
; 
; (deftest item-quantity-must-be-greater-than-zero
;   (binding [*attr* (assoc-in (init-attr) [:items 0 :quantity] 0M)]
;     (failed-validation [:items 0 :quantity]
;                        "Quantity must be greater than zero"
;                        (create-scheduled-transaction))))
; 
; (def ^:private update-context
;   (assoc basic-context
;          :scheduled-transactions [{:entity-id "Personal"
;                                    :description "Paycheck"
;                                    :start-date (t/local-date 2016 1 1)
;                                    :date-spec {:day 1}
;                                    :interval-type :month
;                                    :interval-count 1
;                                    :items [{:action :debit
;                                             :account-id "Checking"
;                                             :quantity 900M}
;                                            {:action :debit
;                                             :account-id "FIT"
;                                             :quantity 100M}
;                                            {:action :credit
;                                             :account-id "Salary"
;                                             :quantity 1000M}]}]))
; 
; (defn- update-sched-tran
;   [update-fn]
;   (let [ctx (realize update-context)
;         sched-tran (find-scheduled-transaction ctx "Paycheck")
;         result (-> sched-tran
;                    (update-fn ctx)
;                    sched-trans/update)
;         retrieved (sched-trans/find sched-tran)]
;     [result retrieved]))
; 
; (deftest update-a-scheduled-transaction
;   (let [changes {:interval-type :week
;                  :interval-count 2}
;         [result retrieved] (update-sched-tran
;                              (fn [sched-tran _]
;                                (-> sched-tran
;                                    (merge changes)
;                                    (assoc-in [:items 0 :quantity] 901M)
;                                    (assoc-in [:items 2 :quantity] 1001M))))]
;     (is (valid? result))
;     (is (map? result) "A map is returned")
;     (is (comparable? changes result) "The updated scheduled-transaction is returned")
;     (is (comparable? changes retrieved) "The record is updated in the database")
;     (is (= #{1001M 901M 100M}
;            (->> (:items retrieved)
;                 (map :quantity)
;                 (into #{})))
;         "The items are updated in the database.")))
; 
; (deftest add-an-item
;   (let [[result retrieved] (update-sched-tran
;                              (fn [sched-tran ctx]
;                                (-> sched-tran
;                                    (assoc-in [:items 0 :quantity] 850M)
;                                    (update-in [:items]
;                                               conj
;                                               {:action :debit
;                                                :account-id (:id (find-account ctx "Medicare"))
;                                                :quantity 50M}))))]
;     (is (valid? result))
;     (is (= (->> (:items retrieved)
;                 (map #(select-keys % [:action :quantity]))
;                 set)
;            #{{:action :credit
;               :quantity 1000M}
;              {:action :debit
;               :quantity 850M}
;              {:action :debit
;               :quantity 100M}
;              {:action :debit
;               :quantity 50M}})
;         "The new item can be retrieved after update")))
; 
; (deftest delete-a-scheduled-transaction
;   (let [ctx (realize update-context)
;         sched-tran (find-scheduled-transaction ctx "Paycheck")
;         _ (sched-trans/delete sched-tran)
;         retrieved (sched-trans/find sched-tran)]
;     (is (nil? retrieved) "The record cannot be retrieved after delete")))
; 
; (defn- realize-tran
;   [date]
;   (let [ctx (realize update-context)
;         sched-tran (find-scheduled-transaction ctx "Paycheck")]
;     [(with-fixed-time date
;        (sched-trans/realize sched-tran))
;      (trans/search {:description "Paycheck"
;                     :transaction-date [:between> (t/local-date 2016 1 1) (t/local-date 2017 1 1)]
;                     :entity-id (:entity-id sched-tran)})
;      (sched-trans/find sched-tran)]))
; 
; (defn- assert-successful-realization
;   [[result transactions sched-tran]]
;   (is (every? :id result)
;       "The returned values contain an :id")
;   (is (valid? result))
;   (is (= (:transaction-date (last transactions))
;          (:last-occurrence sched-tran))
;       "The scheduled transaction last occurrence is updated")
;   (is (= (:id sched-tran)
;          (-> result first :scheduled-transaction-id))
;       "The new transaction has the :scheduled-transaction-id")
;   (let [expected {:transaction-date (t/local-date 2016 2 1)
;                   :description "Paycheck"}]
;     #_{:clj-kondo/ignore [:unused-value]}
;     (= expected
;        (select-keys result (keys expected))
;        "The created transaction is returned")
;     (= expected
;        (select-keys (first transactions) (keys expected))
;        "The transaction can be retrieved")))
; 
; (deftest realize-a-scheduled-transaction-after-the-date
;   (assert-successful-realization (realize-tran "2016-02-02T00:00:00Z")))
