(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.x-platform.util :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [create-fn
                                              update-fn]]))

(s/def ::name string?)
(s/def ::id integer?)
(s/def ::user-id integer?)
(s/def ::monitored-account-ids (s/coll-of integer? :kind set?))
(s/def ::inventory-method #{:fifo :lifo})
(s/def ::default-commodity-id integer?)
(s/def ::settings (s/keys :opt-un [::inventory-method ::monitored-account-ids ::default-commodity-id]))
(s/def ::new-entity (s/keys :req-un [::name ::user-id] :opt-un [::settings]))
(s/def ::existing-entity (s/keys :req-un [::id ::name] :opt-un [::user-id ::settings]))

(defn- after-read
  [entity & _]
  (when entity
    (-> entity
        (storage/tag ::models/entity)
        (update-in-if [:settings] read-string))))

(defn select
  "Returns entities for the specified user"
  ([storage-spec criteria]
   (select storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read
          (storage/select s
                          (storage/tag criteria ::models/entity)
                          options)))))

(defn- name-is-unique?
  [storage {entity-name :name
            user-id :user-id
            entity-id :id}]
  (->> (select storage {:user-id user-id} {})
       (remove #(= (:id %) entity-id))
       (filter #(= (:name %) entity-name))
       empty?))

(defn- before-validation
  [entity & _]
  (update-in entity [:settings] (fnil identity {})))

(defn- before-save
  [entity & _]
  (-> entity
      (storage/tag ::models/entity)
      (update-in-if [:settings :monitored-account-ids] set)
      (update-in-if [:settings] pr-str)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial name-is-unique? storage)
                           [:name]
                           "Name is already in use")])

(def ^:private coercion-rules
  [(coercion/rule :keyword [:settings :inventory-method])])

(def create
  (create-fn {:before-validation before-validation
              :before-save before-save
              :after-read after-read
              :create (fn [entity s] (storage/create s entity))
              :spec ::new-entity
              :rules-fn validation-rules
              :coercion-rules coercion-rules}))

(defn find-by
  "Returns the first entity that matches the specified criteria"
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
  (first (select storage-spec criteria (merge options {:limit 1})))))

(defn find-by-id
  "Finds the entity with the specified ID"
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn reload
  "Reloads the specified entity"
  [storage-spec entity]
  (find-by-id storage-spec (:id entity)))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [storage-spec user entity-name]
  (or
    (find-by storage-spec {:user-id (:id user)
                           :name entity-name})
    (create storage-spec {:user-id (:id user)
                          :name entity-name})))

(def update
  (update-fn {:update (fn [entity s] (storage/update s entity))
              :spec ::existing-entity
              :coercion-rules coercion-rules
              :rule-fn validation-rules
              :before-validation before-validation
              :before-save before-save
              :after-read after-read
              :find find-by-id}))

(defn delete
  "Removes the specifiedy entity and all related records from storage"
  [storage-spec entity]
  (with-storage [s storage-spec]
    (storage/delete s entity)))

(defn entity?
  [model]
  (= ::models/entity (storage/tag model)))
