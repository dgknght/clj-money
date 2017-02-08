(ns clj-money.models.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-commodity
                                              find-commodity-by-id
                                              update-commodity
                                              select-commodities-by-entity-id
                                              delete-commodity]]))

(def exchanges #{:nyse :nasdaq :fund})

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name validation/non-empty-string?)
(s/def ::symbol validation/non-empty-string?)
(s/def ::exchange #{:nyse :nasdaq :fund}) ; TODO need to be able to register custom interpretations to resue exchanges
(s/def ::new-commodity (s/keys :req-un [::entity-id ::name ::symbol ::exchange]))
(s/def ::existing-commodity (s/keys :req-un [::name ::symbol ::exchange] :opt-un [::id]))

(defn- before-save
  [commodity]
  (update-in commodity [:exchange] name))

(defn- after-read
  [commodity]
  (when commodity
    (update-in commodity [:exchange] keyword)))

(def ^:private coercion-rules
  [(coercion/rule :integer [:entity-id])
   (coercion/rule :keyword [:exchange])
   (coercion/rule :integer [:id])])

(defn- name-is-in-use?
  [storage {:keys [id entity-id exchange] commodity-name :name :as commodity}]
  (when (and commodity-name entity-id exchange)
    (->> (select-commodities-by-entity-id
           storage
           entity-id
           {:where {:name commodity-name
                    :exchange (name exchange)}})
         (remove #(= id (:id %)))
         seq)))

(defn- symbol-is-in-use?
  [storage {:keys [id entity-id exchange] commodity-symbol :symbol}]
  (when (and commodity-symbol entity-id exchange)
    (->> (select-commodities-by-entity-id
           storage
           entity-id
           {:where {:symbol commodity-symbol
                    :exchange (name exchange)}})
         (remove #(= id (:id %)))
         seq)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (complement (partial name-is-in-use? storage))
                           [:name]
                           "Name must be unique for a given exchange")
   (validation/create-rule (complement (partial symbol-is-in-use? storage))
                           [:symbol]
                           "Symbol must be unique for a given exchange")])

(defn- before-validation
  [commodity]
  (coercion/coerce commodity coercion-rules))

(defn- validate
  [storage spec commodity]
  (apply validation/validate
         spec
         (before-validation commodity)
         (validation-rules storage)))

(defn create
  "Creates a new commodity record"
  [storage-spec commodity]
  (with-storage [s storage-spec]
    (let [validated (validate s ::new-commodity commodity)]
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

(defn find-by-id
  "Returns the commodity having the specified ID"
  [storage-spec id]
  (with-storage [s storage-spec]
    (after-read (find-commodity-by-id s id))))

(defn update
  "Updates the specified commodity"
  [storage-spec commodity]
  (with-storage [s storage-spec]
    (let [validated (validate s ::existing-commodity commodity)]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save
               (update-commodity s))
          (find-by-id s (:id validated)))
        validated))))

(defn delete
  "Removes a commodity from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-commodity s id)))

(defn create-price
  [storage-spec price]
  )

(defn select-prices-by-commodity-id
  [storage-spec commodity-id]
  [])
