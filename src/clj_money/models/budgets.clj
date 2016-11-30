(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [schema.core :as schema]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage with-transacted-storage]]
            [clj-money.models.storage :refer [create-budget
                                              create-budget-item
                                              find-budget-by-id
                                              find-budget-item-by-id
                                              select-budgets-by-entity-id
                                              select-budget-items-by-budget-id]])
  (:import org.joda.time.LocalDate))

(def Budget
  {:entity-id schema/Int
   :name schema/Str
   :start-date LocalDate
   :period (schema/enum :week :month :quarter)
   :period-count schema/Int})

(def BudgetItemPeriod
  {:index schema/Int
   :amount BigDecimal})

(def BudgetItem
  {:budget-id schema/Int
   :account-id schema/Int
   :periods [BudgetItemPeriod]})

(defn select-by-entity-id
  "Returns the budgets for the specified entity"
  [storage-spec entity-id]
  (with-storage [s storage-spec]
    (select-budgets-by-entity-id s entity-id)))

(defn- before-validation
  [budget]
  budget)

(defn- before-save
  [budget]
  (-> budget
      (update-in [:start-date] tc/to-long)
      (update-in [:period] name)))

(defn- period-count-must-be-greater-than-one
  [{period-count :period-count :as budget}]
  {:model budget
   :errors (if (and period-count
                    (> 1 period-count))
             [[:period-count "Period count must be greater than zero"]]
             [])})

(defn- validation-rules
  [schema]
  [(partial validation/apply-schema schema)
   period-count-must-be-greater-than-one])

(defn- validate
  [schema budget]
  (-> budget
      before-validation
      (validation/validate-model (validation-rules schema))))

(defn- prepare-item-for-return
  [item]
  (update-in item [:periods] read-string))

(defn- select-items-by-budget-id
  [storage budget-id]
  (map prepare-item-for-return
       (select-budget-items-by-budget-id storage budget-id)))

(defn- prepare-for-return
  [storage budget]
  (-> budget
      (update-in [:start-date] tc/to-local-date)
      (assoc :items (select-items-by-budget-id storage (:id budget)))))

(defn create
  "Creates a new budget"
  [storage-spec budget]
  (with-storage [s storage-spec]
    (let [validated (->> budget
                         before-validation
                         (validate Budget))]
      (if (validation/has-error? validated)
        validated
        (->> validated
             before-save
             (create-budget s)
             (prepare-for-return s))))))

(defn find-by-id
  "Returns the specified budget"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> (find-budget-by-id s id)
         (prepare-for-return s))))

(defn- item-validation-rules
  [schema]
  [(partial validation/apply-schema schema)])

(defn- before-item-validation
  [item]
  item)

(defn- validate-item
  [schema item]
  (-> item
      before-item-validation
      (validation/validate-model (item-validation-rules schema))))

(defn- before-save-item
  [item]
  (update-in item [:periods] prn-str))

(defn create-item
  "Adds a new budget item to an existing budget"
  [storage-spec item]
  (let [validated (validate-item BudgetItem item)]
    (if (validation/has-error? validated)
    validated
    (with-storage [s storage-spec]
      (create-budget-item s (before-save-item validated))))))

(defn find-item-by-id
  "Returns the budget item with the specified id"
  [storage-spec item-id]
  (with-storage [s storage-spec]
    (prepare-item-for-return (find-budget-item-by-id s item-id))))

(defn reload
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec budget]
  (find-by-id storage-spec (:id budget)))
