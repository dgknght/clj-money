(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clojure.reflect :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-entity
                                              select-entities
                                              find-entity-by-id
                                              update-entity
                                              delete-entity]]))

(s/def ::name string?)
(s/def ::id integer?)
(s/def ::user-id integer?)
(s/def ::monitored-account-ids (s/coll-of integer?))
(s/def ::new-entity (s/keys :req-un [::name ::user-id] :opt-un [::monitored-account-ids]))
(s/def ::existing-entity (s/keys :req-un [::id ::name] :opt-un [::monitored-account-ids ::user-id]))

(defn- name-is-unique?
  [storage {entity-name :name
            user-id :user-id
            entity-id :id}]
  (->> (select-entities storage user-id)
       (remove #(= (:id %) entity-id))
       (filter #(= (:name %) entity-name))
       empty?))

(defn- before-save
  [entity]
  (cond-> entity

    (contains? entity :monitored-account-ids)
    (update-in [:monitored-account-ids] pr-str)))

(defn create
  "Creates a new entity"
  [storage-spec entity]
  (with-storage [s storage-spec]
    (let [unique-name-rule (validation/create-rule (partial name-is-unique? s)
                                                   [:name]
                                                   "Name is already in use")
          validated (validation/validate ::new-entity entity unique-name-rule)]
      (if (validation/valid? validated)
        (->> validated
             before-save
             (create-entity s))
        validated))))

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
    (let [validated (validation/validate ::existing-entity entity)]
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
