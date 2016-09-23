(ns clj-money.models.entities
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
                                              find-entity-by-id]]))

(def NewEntity
  "Schema for unsaved entities"
  {:name s/Str
   :user-id s/Int})

(def Entity
  "Schema for saved entities"
  {:id s/Int
   :name (s/maybe s/Str)})

(defn- prepare-entity-for-save
  [entity]
  (rename-keys entity {:user-id :user_id}))

(defn- prepare-entity-for-return
  [entity]
  (rename-keys entity {:user_id :user-id}))

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
         prepare-entity-for-save
         (create-entity s)
         prepare-entity-for-return)))

(defn select
  "Returns entities for the specified user"
  [storage-spec user-id]
  (map prepare-entity-for-return
       (select-entities (storage storage-spec)
                        user-id)))

(defn find-by-id
  "Finds the entity with the specified ID"
  [storage-spec id]
  (-> storage-spec
      storage
      (find-entity-by-id id)
      prepare-entity-for-return))

(defn update-entity
  "Updates the specified entity"
  [storage-spec entity]
  (-> storage-spec
      storage
      (validate-existing-entity entity)
      (update-entity entity)))
