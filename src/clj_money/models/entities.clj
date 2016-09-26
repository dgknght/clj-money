(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.reflect :refer :all]
            [schema.core :as s]
            [clj-money.models.helpers :refer [storage
                                              validate-model
                                              throw-validation-exception]]
            [clj-money.models.storage :refer [create-entity
                                              select-entities
                                              entity-exists-with-name?
                                              find-entity-by-id
                                              update-entity
                                              delete-entity]]))

(def NewEntity
  "Schema for unsaved entities"
  {:name s/Str
   :user-id s/Int})

(def Entity
  "Schema for saved entities"
  {:id s/Int
   :name (s/maybe s/Str)})

(defn- validate-entity
  [storage schema entity]
  (let [validated (validate-model entity schema "entity")]
    (if (entity-exists-with-name? storage
                                  (:user-id validated)
                                  (:name validated))
      (throw-validation-exception {:name :duplicate-key}
                                  entity
                                  NewEntity
                                  "entity")
      validated)))

(defn- validate-new-entity
  [storage entity]
  (validate-entity storage NewEntity entity))

(defn- validate-existing-entity
  [storage entity]
  (validate-entity storage Entity entity))

(defn create
  "Creates a new entity"
  [storage-spec entity]
  (let [s (storage storage-spec)]
    (->> entity
         (validate-new-entity s)
         (create-entity s))))

(defn select
  "Returns entities for the specified user"
  [storage-spec user-id]
  (select-entities (storage storage-spec)
                   user-id))

(defn find-by-id
  "Finds the entity with the specified ID"
  [storage-spec id]
  (-> storage-spec
      storage
      (find-entity-by-id id)))

(defn update
  "Updates the specified entity"
  [storage-spec entity]
  (let [s (storage storage-spec)
        validated (validate-existing-entity s entity)]
    (update-entity s entity)))

(defn delete
  "Removes the specifiedy entity from storage"
  [storage-spec id]
  (delete-entity (storage storage-spec) id))
