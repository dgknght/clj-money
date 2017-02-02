(ns clj-money.models.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-commodity
                                              select-commodities-by-entity-id]]))

(s/def ::entity-id integer?)
(s/def ::name validation/non-empty-string?)
(s/def ::new-commodity (s/keys :req-un [::entity-id ::name]))

(defn- before-save
  [commodity]
  (update-in commodity [:exchange] name))

(defn- after-read
  [commodity]
  (update-in commodity [:exchange] keyword))

(def ^:private coercion-rules
  [(coercion/rule :integer [:entity-id])])

(defn- before-validation
  [commodity]
  (coercion/coerce commodity coercion-rules))

(defn- validate
  [commodity]
  (validation/validate ::new-commodity
                       (before-validation commodity)))

(defn create
  "Creates a new commodity record"
  [storage-spec commodity]
  (with-storage [s storage-spec]
    (let [validated (validate commodity)]
      (if (validation/valid? validated)
        (->> validated
             before-save
             (create-commodity s)
             after-read)
        validated))))

(defn select-by-entity-id
  "Returns the commodities belonging to the specified entity"
  [storage-spec entity-id]
  (with-storage [s storage-spec]
    (->> entity-id
         (select-commodities-by-entity-id s)
         (map after-read))))
