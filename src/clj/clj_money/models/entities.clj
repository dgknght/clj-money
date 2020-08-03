(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.util :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.validation :as validation :refer [with-validation]]))

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
  [entity]
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
  [entity]
  (update-in entity [:settings] (fnil identity {})))

(defn- before-save
  [entity]
  (-> entity
      (storage/tag ::models/entity)
      (update-in-if [:settings :monitored-account-ids] set)
      (update-in-if [:settings] pr-str)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial name-is-unique? storage)
                           [:name]
                           "Name is already in use")])

(defn create
  [storage entity]
  (with-storage [s storage]
    (let [entity (before-validation entity)]
      (with-validation entity ::new-entity (validation-rules s)
        (as-> entity e
          (before-save e)
          (storage/create s e)
          (after-read e))))))

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

(defn update
  [storage entity]
  (with-storage [s storage]
    (let [entity (before-validation entity)]
      (with-validation entity ::existing-entity (validation-rules s)
        (storage/update s (before-save entity))
        (find-by-id s (:id entity))))))

(defn delete
  "Removes the specifiedy entity and all related records from storage"
  [storage-spec entity]
  (with-storage [s storage-spec]
    (storage/delete s entity)))

(defn entity?
  [model]
  (= ::models/entity (storage/tag model)))
