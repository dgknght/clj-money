(ns clj-money.models.commodities
  (:refer-clojure :exclude [update count find])
  (:require [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.util :refer [update-in-if
                                    assoc-if
                                    ->id]]
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
  [commodity]
  (-> commodity
      (tag ::models/commodity)
      (update-in-if [:exchange] name)
      (update-in [:type] name)))

(defn- after-read
  [commodity]
  (when commodity
    (-> commodity
        (tag ::models/commodity)
        (update-in-if [:exchange] keyword)
        (update-in [:type] keyword))))

(defn search
  "Returns commodities matching the specified criteria"
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/commodity)
                          options)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (merge options {:limit 1})))))

(defn find
  "Returns the commodity having the specified ID"
  [id-or-commodity]
  (find-by {:id (->id id-or-commodity)}))

(defn- name-is-unique?
  [{:keys [id entity-id exchange] commodity-name :name}]
  (-> {:entity-id entity-id
       :name commodity-name
       :exchange (when exchange (name exchange))}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))

(defn- symbol-is-unique?
  [{:keys [id entity-id exchange] commodity-symbol :symbol}]
  (-> {:entity-id entity-id
       :symbol commodity-symbol
       :exchange (when exchange (name exchange))}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))

(def ^:private validation-rules
  [(validation/create-rule name-is-unique?
                           [:name]
                           "Name must be unique for a given exchange")
   (validation/create-rule symbol-is-unique?
                           [:symbol]
                           "Symbol must be unique for a given exchange")])

(defn- set-implicit-default
  "After a commodity is saved, checks to see if it is
  the only currency commodity. If so, update the entity to indicate
  this is the default."
  [commodity]
  (when (#{:currency "currency"} (:type commodity))
    (let [other-commodities (search {:entity-id (:entity-id commodity)
                                     :id [:!= (:id commodity)]})]
      (when (empty? other-commodities)
        (let [entity (entities/find (:entity-id commodity))]
          (entities/update (assoc-in entity
                                     [:settings :default-commodity-id]
                                     (:id commodity)))))))
  commodity)

(defn create
  [commodity]
  (with-storage (env :db)
    (with-validation commodity ::new-commodity validation-rules
      (-> commodity
          before-save
          storage/create
          set-implicit-default
          after-read))))

(defn count
  "Returns the number of commodities matching the specified criteria"
  [criteria]
  (-> (search criteria {:count true})
      first
      :record-count))

(defn update
  [commodity]
  (with-storage (env :db)
    (with-validation commodity ::existing-commodity validation-rules
      (-> commodity
          before-save
          storage/update)
      (find commodity))))

(defn delete
  "Removes a commodity from the system"
  [commodity]
  (with-storage (env :db)
    (storage/delete commodity)))
