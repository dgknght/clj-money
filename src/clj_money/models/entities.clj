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

(def Entity
  "Schema for entities"
  {:name s/Str
   :user-id s/Int})

(defn- prepare-entity-for-save
  [entity]
  (rename-keys entity {:user-id :user_id}))

(defn- prepare-entity-for-return
  [entity]
  (rename-keys entity {:user_id :user-id}))

(defn- validate-entity
  [storage entity]
  (let [validated (validate-model entity Entity "entity")]
    (if (entity-exists-with-name? storage
                                  (:user-id validated)
                                  (:name validated))
      (throw-validation-exception {:name :duplicate-key}
                                  entity
                                  Entity
                                  "entity")
      validated)))

(defn create
  "Creates a new entity"
  [storage-spec entity]
  (let [s (storage storage-spec)]
    (->> entity
         (validate-entity s)
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
