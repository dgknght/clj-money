(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.reflect :refer :all]
            [schema.core :as s]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [with-storage]]
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
   (s/optional-key :user-id) s/Int
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
  (with-storage [s storage-spec]
    (let [validated (validate-new-entity s entity)]
      (when (not validated)
        (throw (ex-info "The validated data is null" {:entity entity
                                                      :validated validated})))
      (if (validation/has-error? validated)
        validated
        (create-entity s validated)))))

(defn select
  "Returns entities for the specified user"
  [storage-spec user-id]
  (with-storage [s storage-spec]
    (select-entities s user-id)))

(defn find-by-id
  "Finds the entity with the specified ID"
  [storage-spec id]
  (with-storage [s storage-spec]
    (find-entity-by-id s id)))

(defn find-by-name
  "Finds the entity having the specified name
  for the specified user"
  [storage-spec user entity-name]
  (->> (select storage-spec (:id user))
       (filter #(= entity-name (:name %)))
       first))

(defn update
  "Updates the specified entity"
  [storage-spec entity]
  (with-storage [s storage-spec]
    (let [validated (validate-existing-entity s entity)]
      (if (validation/valid? validated)
        (do
          (update-entity s validated)
          (find-by-id storage-spec (:id validated)))
        validated))))

(defn delete
  "Removes the specifiedy entity from storage"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-entity s id)))
