(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [schema.core :as schema]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage with-transacted-storage]]
            [clj-money.models.storage :refer [create-budget
                                              select-budgets-by-entity-id]])
  (:import org.joda.time.LocalDate))

(def Budget
  {:entity-id schema/Int
   :name schema/Str
   :start-date LocalDate
   :period (schema/enum :week :month :quarter)
   :period-count schema/Int})

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

(defn- prepare-for-return
  [budget]
  (update-in budget [:start-date] tc/to-local-date))

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
             prepare-for-return)))))
