(ns clj-money.models.scheduled-transactions-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.models :as models]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
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

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:scheduled-transaction/entity
                                      [:scheduled-transaction/items :scheduled-transaction-item/account]]))

(deftest create-a-scheduled-transaction
  (with-context
    (assert-created (attributes))))

(deftest entity-id-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/entity)
                    {:scheduled-transaction/entity ["Entity is required"]})))

(deftest description-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/description)
                    {:scheduled-transaction/description ["Description is required"]})))

(deftest interval-type-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/interval-type)
                    {:scheduled-transaction/interval-type ["Interval type is required"]})))

(deftest interval-type-can-be-week
  (with-context
    (assert-created (assoc (attributes) :scheduled-transaction/interval-type :week))))

(deftest interval-type-can-be-month
  (with-context
    (assert-created (assoc (attributes) :scheduled-transaction/interval-type :month))))

(deftest interval-type-can-be-year
  (with-context
    (assert-created (assoc (attributes) :scheduled-transaction/interval-type :year))))

(deftest interval-type-cannot-be-off-list
  (with-context
    (assert-invalid
      (assoc (attributes) :scheduled-transaction/interval-type :not-valid)
      {:scheduled-transaction/interval-type ["Interval type must be day, week, month, or year"]})))

(deftest start-date-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/start-date)
                    {:scheduled-transaction/start-date ["Start date is required"]})))

(deftest date-spec-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/date-spec)
                    {:scheduled-transaction/date-spec ["Date spec is required"]})))

(deftest interval-count-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/interval-count)
                    {:scheduled-transaction/interval-count ["Interval count is required"]})))

(deftest interval-must-be-greater-than-zero
  (with-context
    (assert-invalid
      (assoc (attributes) :scheduled-transaction/interval-count 0)
      {:scheduled-transaction/interval-count ["Interval count must be greater than zero"]})))

(deftest at-least-two-items-are-required
  (with-context
    (assert-invalid
      (update-in (attributes) [:scheduled-transaction/items] #(take 1 %))
      {:scheduled-transaction/items ["Items must contain at least 2 item(s)"]})))

(deftest sum-of-credits-must-equal-sum-of-debits
  (with-context
    (assert-invalid
      (assoc-in (attributes)
                [:scheduled-transaction/items
                 0
                 :scheduled-transaction-item/quantity]
                1001M)
      {:scheduled-transaction/items ["The sum of debits must equal the sum of credits"]})))

(deftest item-account-id-is-required
  (with-context
    (assert-invalid
      (update-in (attributes)
                 [:scheduled-transaction/items 0]
                 dissoc
                 :scheduled-transaction-item/account)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/account
         ["Account is required"]}}})))

(deftest item-action-is-required
  (with-context
    (assert-invalid
      (update-in (attributes)
                 [:scheduled-transaction/items 0]
                 dissoc
                 :scheduled-transaction-item/action)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/action
         ["Action is required"]}}})))

(deftest item-action-must-be-credit-or-debit
  (with-context
    (assert-invalid
      (assoc-in
        (attributes)
        [:scheduled-transaction/items
         0
         :scheduled-transaction-item/action]
        :not-valid)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/action
         ["Action is invalid"]}}})))

(deftest item-quantity-is-required
  (with-context
    (assert-invalid
      (update-in (attributes)
                 [:scheduled-transaction/items 0]
                 dissoc
                 :scheduled-transaction-item/quantity)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/quantity
         ["Quantity is required"]}}})))

(deftest item-quantity-must-be-greater-than-zero
  (with-context
    (assert-invalid
      (assoc-in
        (attributes)
        [:scheduled-transaction/items
         0
         :scheduled-transaction-item/quantity]
        0M)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/quantity
         ["Quantity must be greater than zero"]}}})))

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
