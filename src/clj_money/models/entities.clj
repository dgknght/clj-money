(ns clj-money.models.entities
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.reflect :refer :all]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-entity
                                              select-entities]]))

(defn- prepare-entity-for-save
  [entity]
  (rename-keys entity {:user-id :user_id}))

(defn- prepare-entity-for-return
  [entity]
  (rename-keys entity {:user_id :user-id}))

(defn create
  "Creates a new entity"
  [storage-spec entity]
  (->> entity
       ;validate-entity
       prepare-entity-for-save
       (create-entity (storage storage-spec))
       prepare-entity-for-return))

(defn select
  "Returns entities for the specified user"
  [storage-spec user-id]
  (map prepare-entity-for-return
       (select-entities (storage storage-spec)
                        user-id)))
