(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.reflect :refer :all]
            [schema.core :as s]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [storage]]
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
   :name (s/maybe s/Str)
   (s/optional-key :created-at) s/Any
   (s/optional-key :updated-at) s/Any})

(defn validation-rules
  [schema storage]
  [(partial validation/apply-schema schema)
   (fn [{:keys [user-id name] :as model}]
     {:model model
      :errors (if (and name
                       user-id
                       (entity-exists-with-name? storage user-id name))
                [[:name "Name is already in use"]]
                [])})])

(defn- validate-entity
  [storage schema entity]
  (validation/validate-model entity (validation-rules Entity storage)))

(defn- validate-new-entity
  [storage entity]
  (validation/validate-model entity (validation-rules NewEntity storage)))

(defn- validate-existing-entity
  [storage entity]
  (validate-entity storage Entity entity))

(defn create
  "Creates a new entity"
  [storage-spec entity]
  (let [s (storage storage-spec)
        validated (validate-new-entity s entity)]
    (when (not validated)
      (throw (ex-info "The validated data is null" {:entity entity
                                                    :validated validated})))
    (if (validation/has-error? validated)
      validated
      (create-entity s validated))))

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
