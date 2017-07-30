(ns clj-money.models.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.storage :refer [create-commodity
                                              find-commodity-by-id
                                              update-commodity
                                              select-commodities-by-entity-id
                                              select-commodities
                                              delete-prices-by-commodity-id
                                              delete-commodity]]))

(def exchanges #{:nyse :nasdaq :fund})

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name validation/non-empty-string?)
(s/def ::symbol validation/non-empty-string?)
(s/def ::exchange #{:nyse :nasdaq})
(s/def ::type #{:currency :stock :fund})
(defmulti new-commodity-spec :type)
(defmethod new-commodity-spec :stock [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol ::exchange]))
(defmethod new-commodity-spec :fund [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol]))
(defmethod new-commodity-spec :currency [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol]))
(s/def ::new-commodity (s/multi-spec new-commodity-spec :type))
(s/def ::existing-commodity (s/keys :req-un [::type ::entity-id ::name ::symbol] :opt-un [::id]))

(defn- before-save
  [commodity]
  (-> commodity
      (update-in [:exchange] name)
      (update-in [:type] name)))

(defn- after-read
  [commodity]
  (when commodity
    (-> commodity
        (update-in [:exchange] keyword)
        (update-in [:type] keyword))))

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
  (coercion/coerce coercion-rules commodity))

(defn- validate
  [storage spec commodity]
  (->> commodity
       before-validation
       (validation/validate
         spec
         (validation-rules storage))))

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

(defn search
  "Returns commodities matching the specified criteria"
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (->> criteria
         (select-commodities s)
         (map after-read))))

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
  (with-transacted-storage [s storage-spec]
    (delete-prices-by-commodity-id s id)
    (delete-commodity s id)))
