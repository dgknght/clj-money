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
   :period schema/Keyword
   :period_count schema/Int})

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
  [budget]
  {:model budget
   :errors []})

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
    (->> budget
         before-save
         (create-budget s))))
