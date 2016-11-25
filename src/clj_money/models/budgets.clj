(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [schema.core :as schema]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage with-transacted-storage]]))

(defn select-by-entity-id
  "Returns the budgets for the specified entity"
  [storage-spec entity-id]
  []
  )

(defn create
  "Creates a new budget"
  [storage-spec budget]
  )
