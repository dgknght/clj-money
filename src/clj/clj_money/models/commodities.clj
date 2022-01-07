(ns clj-money.models.commodities
  (:refer-clojure :exclude [update count find])
  (:require [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.core :refer [update-in-if
                                          assoc-if
                                          present? ]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.models.sql-storage-ref]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]))

(declare find-by)

(defn- name-is-unique?
  [{:keys [id] :as commodity}]
  (nil?
    (find-by
      (-> commodity
          (select-keys [:name :exchange :entity-id])
          (assoc-if :id (when id [:!= id]))))))

(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:name]})

(defn- symbol-is-unique?
  [{:keys [id] :as commodity}]
  (nil?
    (find-by
      (-> commodity
          (select-keys [:symbol :exchange :entity-id])
          (assoc-if :id (when id [:!= id]))))))

(v/reg-spec symbol-is-unique? {:message "%s is already in use"
                               :path [:symbol]})

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name (s/and string?
                     present?))
(s/def ::symbol (s/and string?
                     present?))
(s/def ::type #{:currency :stock :fund})

(s/def ::enabled boolean?)
(s/def ::price-config (s/keys :req-un [::enabled]))

(defmulti new-commodity-spec :type)
(defmethod new-commodity-spec :default [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol ::price-config]))
(defmethod new-commodity-spec :stock [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol ::price-config ::models/exchange]))
(defmethod new-commodity-spec :fund [_]
  (s/keys :req-un [::type ::entity-id ::name ::symbol ::price-config ::models/exchange]))

(s/def ::new-commodity (s/and (s/keys :req-un [::type])
                              (s/multi-spec new-commodity-spec :type)
                              name-is-unique?
                              symbol-is-unique?))

(s/def ::existing-commodity (s/keys :req-un [::id ::type ::entity-id ::name ::symbol ::price-config]))

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
        (update-in [:type] keyword)
        (update-in [:price-config] #(transform-keys ->kebab-case-keyword %)))))

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
    (with-validation commodity ::new-commodity
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
    (with-validation commodity ::existing-commodity
      (-> commodity
          before-save
          storage/update)
      (find commodity))))

(defn delete
  "Removes a commodity from the system"
  [commodity]
  (with-storage (env :db)
    (storage/delete commodity)))
