(ns clj-money.models.commodities
  (:refer-clojure :exclude [update count])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core
             :as storage
             :refer [with-storage]]
            [clj-money.x-platform.util :refer [update-in-if]]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]))

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name validation/non-empty-string?)
(s/def ::symbol validation/non-empty-string?)
(s/def ::exchange #{:nyse :nasdaq :amex})
(s/def ::type #{:currency :stock :fund})

(def exchanges #{:nyse :nasdaq})

(defmulti new-commodity-spec :type)
(defmethod new-commodity-spec :default [_]
  (s/keys :req-un [::type]))
(defmethod new-commodity-spec :stock [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol ::exchange]))
(defmethod new-commodity-spec :fund [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol]))
(defmethod new-commodity-spec :currency [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol]))

(s/def ::new-commodity (s/multi-spec new-commodity-spec :type))

(s/def ::existing-commodity (s/keys :req-un [::type ::entity-id ::name ::symbol] :opt-un [::id]))

(defn- before-save
  [commodity & _]
  (-> commodity
      (storage/tag ::models/commodity)
      (update-in-if [:exchange] name)
      (update-in [:type] name)))

(defn- after-read
  [commodity & _]
  (when commodity
    (-> commodity
        (storage/tag ::models/commodity)
        (update-in-if [:exchange] keyword)
        (update-in [:type] keyword))))

(defn search
  "Returns commodities matching the specified criteria"
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read
          (storage/select s
                          (storage/tag criteria ::models/commodity)
                          options)))))

(defn- name-is-in-use?
  [storage {:keys [id entity-id exchange] commodity-name :name}]
  (when (and commodity-name entity-id exchange)
    (->> (search
           storage
           {:entity-id entity-id
            :name commodity-name
            :exchange (name exchange)})
         (remove #(= id (:id %)))
         seq)))

(defn- symbol-is-in-use?
  [storage {:keys [id entity-id exchange] commodity-symbol :symbol}]
  (when (and commodity-symbol entity-id exchange)
    (->> (search
           storage
           {:entity-id entity-id
            :symbol commodity-symbol
            :exchange (name exchange)})
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

(defn- set-implicit-default
  "After a commodity is saved, checks to see if it is
  the only currency commodity. If so, update the entity to indicate
  this is the default."
  [commodity storage]
  (when (#{:currency "currency"} (:type commodity))
    (let [other-commodities (search storage
                                    {:entity-id (:entity-id commodity)
                                     :id [:!= (:id commodity)]})]
      (when (empty? other-commodities)
        (let [entity (entities/find-by-id storage (:entity-id commodity))]
          (entities/update storage (assoc-in entity
                                             [:settings :default-commodity-id]
                                             (:id commodity)))))))
  commodity)

(defn create
  [storage commodity]
  (with-storage [s storage]
    (with-validation commodity ::new-commodity (validation-rules s)
      (as-> commodity c
        (before-save c)
        (storage/create s c)
        (set-implicit-default c s)
        (after-read c)))))

(defn count
  "Returns the number of commodities matching the specified criteria"
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (-> (search s criteria {:count true})
        first
        :record-count)))

(defn find-by
  [storage-spec criteria]
  (first (search storage-spec criteria {:limit 1})))

(defn find-by-id
  "Returns the commodity having the specified ID"
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn update
  [storage commodity]
  (with-storage [s storage]
    (with-validation commodity ::existing-commodity (validation-rules s)
      (storage/update s (before-save commodity))
      (find-by-id s (:id commodity)))))

(defn delete
  "Removes a commodity from the system"
  [storage-spec commodity]
  (with-storage [s storage-spec]
    (storage/delete s commodity)))
