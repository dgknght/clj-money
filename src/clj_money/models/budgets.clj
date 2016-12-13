(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-time.coerce :as tc]
            [schema.core :as schema]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage with-transacted-storage]]
            [clj-money.models.storage :refer [create-budget
                                              update-budget
                                              create-budget-item
                                              update-budget-item
                                              find-budget-by-id
                                              find-budget-item-by-id
                                              select-budgets-by-entity-id
                                              select-budget-items-by-budget-id
                                              delete-budget]])
  (:import org.joda.time.LocalDate))

(def BudgetBase
  {:name schema/Str
   :start-date LocalDate
   :period (schema/enum :week :month :quarter)
   :period-count schema/Int})

(def NewBudget
  (merge BudgetBase
         {:entity-id schema/Int}))

(def ExistingBudget
  (merge BudgetBase {:id schema/Int}))

(def BudgetItemPeriod
  {:index schema/Int
   :amount BigDecimal})

(def BaseBudgetItem
  {:account-id schema/Int
   :periods [BudgetItemPeriod]})

(def NewBudgetItem
  (merge BaseBudgetItem {:budget-id schema/Int}))

(def ExistingBudgetItem
  (merge BaseBudgetItem {:id schema/Int}))

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
      (update-in [:period] keyword)
      (assoc :items (select-items-by-budget-id storage (:id budget)))))

(defn select-by-entity-id
  "Returns the budgets for the specified entity"
  [storage-spec entity-id]
  (with-storage [s storage-spec]
    (map #(prepare-for-return s %) (select-budgets-by-entity-id s entity-id))))

(defn- before-validation
  [budget]
  (dissoc budget :items))

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

(defn create
  "Creates a new budget"
  [storage-spec budget]
  (with-storage [s storage-spec]
    (let [validated (validate NewBudget budget)]
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

(defn reload
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec budget]
  (with-storage [s storage-spec]
    (->> budget
         :id
         (find-by-id s)
         (prepare-for-return s))))

(defn update
  "Updates the specified budget"
  [storage-spec budget]
  (with-storage [s storage-spec]
    (let [validated (validate ExistingBudget
                              (select-keys budget [:id
                                                   :name
                                                   :period
                                                   :period-count
                                                   :start-date]))]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save
               (update-budget s))
          (reload s validated))
        validated))))

(defn find-item-by-id
  "Returns the budget item with the specified id"
  [storage-spec item-id]
  (with-storage [s storage-spec]
    (->> item-id
         (find-budget-item-by-id s)
         prepare-item-for-return)))

(defn- budget-item-account-belongs-to-budget-entity
  [storage budget item]
  {:model item
   :errors (if-let [account (accounts/find-by-id storage (:account-id item))]
             (let [entity-ids (->> [account budget]
                                   (map :entity-id)
                                   (into #{}))]
               (if (= 1 (count entity-ids))
                 []
                 [[:account-id "Account must belong to the same entity as the budget"]]))
             [])})

(defn- budget-item-has-correct-number-of-periods
  [storage budget item]
  {:model item
   :errors (if (= (:period-count budget) (count (:periods item)))
             []
             ; the extra square brackets here are a bit of a hack
             [[:periods ["Number of periods must match the budget \"Period count\" value"]]])})

(defn- budget-item-account-is-unique
  [storage budget item]
  {:model item
   :errors (if (->> (:items budget)
                    (filter #(= (:account-id item) (:account-id %)))
                    (remove #(= (:id item) (:id %)))
                    seq)
             [[:account-id "Account is already in the budget"]]
             [])})

(defn- item-validation-rules
  [storage schema budget]
  [(partial validation/apply-schema schema)
   (partial budget-item-account-is-unique storage budget)
   (partial budget-item-account-belongs-to-budget-entity storage budget)
   (partial budget-item-has-correct-number-of-periods storage budget)])

(defn- before-item-validation
  [item]
  item)

(defn- validate-item
  [storage schema budget item]
  (-> item
      before-item-validation
      (validation/validate-model (item-validation-rules storage schema budget))))

(defn- before-save-item
  [item]
  (update-in item [:periods] prn-str))

(defn create-item
  "Adds a new budget item to an existing budget"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [budget (when (:budget-id item) (find-by-id s (:budget-id item)))
          validated (validate-item s NewBudgetItem budget item)]
      (if (validation/has-error? validated)
        validated
        (->> validated
             before-save-item
             (create-budget-item s)
             prepare-item-for-return)))))

(defn- reload-item
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec item]
  (with-storage [s storage-spec]
    (->> item
         :id
         (find-item-by-id s))))

(defn update-item
  "Updates the specified budget item"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [budget (->> item
                      :id
                      (Integer.)
                      (find-item-by-id s)
                      :budget-id
                      (find-by-id s))
          validated (validate-item s
                                   ExistingBudgetItem
                                   budget
                                   (select-keys item [:id
                                                      :account-id
                                                      :periods]))]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save-item
               (update-budget-item s))
          (reload-item s validated))
        validated))))

(defn delete
  "Removes the specified budget from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-budget s id)))
