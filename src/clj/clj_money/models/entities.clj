(ns clj-money.models.entities
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :refer [keywordize-keys]]
            [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.util :refer [update-in-if
                                    assoc-if
                                    ->id]]
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
        (update-in-if [:settings] keywordize-keys)
        (update-in-if [:settings :monitored-account-ids] set)
        (update-in-if [:settings :inventory-method] keyword)
        (tag ::models/entity))))

(defn select
  "Returns entities for the specified user"
  ([criteria]
   (select criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/entity)
                          options)))))

(defn find-by
  "Returns the first entity that matches the specified criteria"
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (select criteria (merge options {:limit 1})))))

(defn find
  "Finds the entity with the specified ID"
  [id-or-entity]
  (find-by {:id (->id id-or-entity)}))

(defn reload
  "Reloads the specified entity"
  [entity]
  (find entity))

(defn- name-is-unique?
  [{:keys [name id user-id]}]
  (-> {:name name
       :user-id user-id}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))

(defn- before-validation
  [entity]
  (update-in entity [:settings] (fnil identity {})))

(defn- before-save
  [entity]
  (-> entity
      (tag ::models/entity)
      (update-in-if [:settings :monitored-account-ids] set)))

(def ^:private validation-rules
  [(validation/create-rule name-is-unique?
                           [:name]
                           "Name is already in use")])

(defn create
  [entity]
  (with-storage (env :db)
    (let [entity (before-validation entity)]
      (with-validation entity ::new-entity validation-rules
        (-> entity
            before-save
            storage/create
            after-read)))))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [user entity-name]
  (or
   (find-by {:user-id (:id user)
             :name entity-name})
   (create {:user-id (:id user)
            :name entity-name})))

(defn update
  [entity]
  (with-storage (env :db)
    (let [entity (before-validation entity)]
      (with-validation entity ::existing-entity validation-rules
        (-> entity
            before-save
            storage/update)
        (find entity)))))

(defn delete
  "Removes the specifiedy entity and all related records from storage"
  [entity]
  (with-storage (env :db)
    (storage/delete entity)))

(defn entity?
  [model]
  (= ::models/entity (tag model)))
