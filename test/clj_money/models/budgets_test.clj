(ns clj-money.models.budgets-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def budget-context
  {:users [{:email "john@doe.com"
            :first-name "John"
            :last-name "Doe"
            :password "please01"}]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]})

(defn- attributes
  [context]
  {:entity-id (-> context :entities first :id)
   :name "2017"
   :start-date (t/local-date 2017 1 1)
   :period :month
   :period-count 12})

(deftest create-a-budget
  (let [context (serialization/realize storage-spec budget-context)
        [entity] (:entities context)
        budget (budgets/create
                 storage-spec
                 (attributes context))
        budgets (budgets/select-by-entity-id storage-spec (:id entity))]
    (is (not (nil? (:id budget))) "The returned value has an id")
    (is (= ["2017"] (map :name budgets))
        "A query for budgets returns the new budget")))

(deftest entity-id-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)]
    (assert-validation-error
      :entity-id
      "Entity id is required"
      (budgets/create storage-spec (dissoc (attributes context) :entity-id)))))

(deftest name-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :name))]
    (assert-validation-error
      :name
      "Name is required"
      result)))

(deftest start-date-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :start-date))]
    (assert-validation-error
      :start-date
      "Start date is required"
      result)))

(deftest start-date-can-be-a-string
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :start-date "3/2/2016"))]
    (is (validation/valid? result) "The budget should be valid with a string start date.")
    (is (= (t/local-date 2016 3 2) (:start-date result)) "The result should have the correct start date value")))

(deftest period-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :period))]
    (assert-validation-error
      :period
      "Period is required"
      result)))

(deftest period-must-be-week-month-or-quarter
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :period :not-a-period))]
    (assert-validation-error
      :period
      "Period must be one of: quarter, week, month"
      result)))

(deftest period-count-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :period-count))]
    (assert-validation-error
      :period-count
      "Period count is required"
      result)))

(deftest period-count-must-be-greater-than-zero
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :period-count 0))]
    (assert-validation-error
      :period-count
      "Period count must be greater than zero"
      result)))

;; Items
(def budget-item-context
  {:users [{:email "john@doe.com"
            :first-name "John"
            :last-name "Doe"
            :password "please01"}]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]
   :accounts [{:name "Salary"
               :type :income}
              {:name "Rent"
               :type :expense}
              {:name "Groceries"
               :type :expense}]
   :budgets [{:name "2017"
              :start-date (t/local-date 2017 1 1)
              :period :month
              :period-count 12}]})

(defn- budget-item-attributes
  [context]
  {:account-id (-> context :accounts second :id)
   :budget-id (-> context :budgets first :id)
   :periods (mapv #(hash-map :amount (bigdec 700)
                             :index %)
                  (range 12))})

(deftest create-budget-item
  (let [context (serialization/realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (budget-item-attributes context))
        budget (budgets/reload storage-spec (-> context :budgets first))]
    (is (validation/valid? item) "The new item is valid")
    (is (not (nil? (:id item))) "The new item has an id value")
    (is (= 1 (-> budget :items count)) "The item is returned with the budget after create")))

; budget-id is required
; account-id is required
; account must be in the same entity as the budget
; must have same number of budget-periods as budget period-count

;; Periods
; budget-item-id is required
; amount is required
; index is required
