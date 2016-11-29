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

(def create-context
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
  (let [context (serialization/realize storage-spec create-context)
        [entity] (:entities context)
        budget (budgets/create
                 storage-spec
                 (attributes context))
        budgets (budgets/select-by-entity-id storage-spec (:id entity))]
    (is (not (nil? (:id budget))) "The returned value has an id")
    (is (= ["2017"] (map :name budgets))
        "A query for budgets returns the new budget")))

(deftest entity-id-is-required
  (let [context (serialization/realize storage-spec create-context)
        entity (-> context :entities first)]
    (assert-validation-error
      :entity-id
      "Entity id is required"
      (budgets/create storage-spec (dissoc (attributes context) :entity-id)))))

(deftest name-is-required
  (let [context (serialization/realize storage-spec create-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :name))]
    (assert-validation-error
      :name
      "Name is required"
      result)))

(deftest start-date-is-required
  (let [context (serialization/realize storage-spec create-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :start-date))]
    (assert-validation-error
      :start-date
      "Start date is required"
      result)))

(deftest start-date-can-be-a-string
  (let [context (serialization/realize storage-spec create-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :start-date "3/2/2016"))]
    (is (validation/valid? result) "The budget should be valid with a string start date.")
    (is (= (t/local-date 2016 3 2) (:start-date result)) "The result should have the correct start date value")))

(deftest period-is-required
  (let [context (serialization/realize storage-spec create-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :period))]
    (assert-validation-error
      :period
      "Period is required"
      result)))

(deftest period-must-be-week-month-or-quarter
  (let [context (serialization/realize storage-spec create-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :period :not-a-period))]
    (assert-validation-error
      :period
      "Period must be one of: quarter, week, month"
      result)))

; period is required
; period must be :month :week or :quarter
; period-count is required
; period-count must be greater than zero
; items are required
; each item just have number of items equal to period count
