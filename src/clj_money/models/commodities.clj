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
(s/def ::symbol validation/non-empty-string?)
(s/def ::exchange #{:nyse :nasdaq :fund})
(s/def ::new-commodity (s/keys :req-un [::entity-id ::name ::symbol ::exchange]))

(defn- before-save
  [commodity]
  (update-in commodity [:exchange] name))

(defn- after-read
  [commodity]
  (update-in commodity [:exchange] keyword))

(def ^:private coercion-rules
  [(coercion/rule :integer [:entity-id])])

(defn- name-is-in-use?
  [storage {:keys [id entity-id exchange] commodity-name :name}]
  (when (and commodity-name entity-id exchange)
                 (->> (select-commodities-by-entity-id
                        storage
                        entity-id
                        {:where {:name commodity-name
                                 :exchange (name exchange)}})
                      (remove #(= id (:id %)))
                      seq)))

(defn- validation-rules
  [storage]
  [(validation/create-rule #(complement (name-is-in-use? storage %))
                           [:name]
                           "Name must be unique for a given exchange")])

(defn- before-validation
  [commodity]
  (coercion/coerce commodity coercion-rules))

(defn- validate
  [storage commodity]
  (apply validation/validate
         ::new-commodity
         (before-validation commodity)
         (validation-rules storage)))

(defn create
  "Creates a new commodity record"
  [storage-spec commodity]
  (with-storage [s storage-spec]
    (let [validated (validate s commodity)]
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
