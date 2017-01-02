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

(def BaseEntity
  "Shared entity attributes"
  {:name s/Str
   (s/optional-key :monitored-account-ids) [s/Int]})

(def NewEntity
  "Schema for unsaved entities"
  (merge BaseEntity
         {:user-id s/Int}))

(def Entity
  "Schema for saved entities"
  (merge BaseEntity
         {:id s/Int
          (s/optional-key :user-id) s/Int
          (s/optional-key :created-at) s/Any
          (s/optional-key :updated-at) s/Any}))

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

(defn- before-save
  [entity]
  (cond-> entity

    (contains? entity :monitored-account-ids)
    (update-in [:monitored-account-ids] pr-str)))

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
        (->> validated
             before-save
             (create-entity s ))))))

(defn select
  "Returns entities for the specified user"
  [storage-spec user-id]
  (with-storage [s storage-spec]
    (select-entities s user-id)))

(defn- prepare-for-return
  [entity]
  (cond-> entity
    (:monitored-account-ids entity)
    (update-in [:monitored-account-ids] read-string)))

(defn find-by-id
  "Finds the entity with the specified ID"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> (find-entity-by-id s id)
         prepare-for-return)))

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
          (->> validated
               before-save
               (update-entity s))
          (find-by-id storage-spec (:id validated)))
        validated))))

(defn delete
  "Removes the specifiedy entity from storage"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-entity s id)))
