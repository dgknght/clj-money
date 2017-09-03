(ns clj-money.models.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clojure.reflect :refer :all]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-entity
                                              select-entities
                                              find-entity-by-id
                                              update-entity
                                              delete-entity]]))

(s/def ::name string?)
(s/def ::id integer?)
(s/def ::user-id integer?)
(s/def ::monitored-account-ids (s/coll-of integer?))
(s/def ::inventory-method #{:fifo :lifo})
(s/def ::settings (s/keys :opt-un [::inventory-method ::monitored-account-id]))
(s/def ::new-entity (s/keys :req-un [::name ::user-id] :opt-un [::settings]))
(s/def ::existing-entity (s/keys :req-un [::id ::name] :opt-un [::user-id ::settings]))

(defn- name-is-unique?
  [storage {entity-name :name
            user-id :user-id
            entity-id :id}]
  (->> (select-entities storage user-id)
       (remove #(= (:id %) entity-id))
       (filter #(= (:name %) entity-name))
       empty?))

(defn- before-save
  ([entity] (before-save nil entity))
  ([_ entity]
   (cond-> entity
     (contains? entity :settings)
     (update-in [:settings] pr-str))))

(defn- after-read
  ([entity] (after-read nil entity))
  ([_ entity]
   (when entity
     (cond-> entity
       (:settings entity)
       (update-in [:settings] read-string)))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial name-is-unique? storage)
                           [:name]
                           "Name is already in use")])

(def ^:private coercion-rules
  [(coercion/rule :keyword [:inventory-method])])

(def create
  (create-fn {:before-save before-save
              :after-read after-read
              :create create-entity
              :spec ::new-entity
              :rules-fn validation-rules
              :coercion-rules coercion-rules}))

(defn select
  "Returns entities for the specified user"
  [storage-spec user-id]
  (with-storage [s storage-spec]
    (select-entities s user-id)))

(defn find-by-id
  "Finds the entity with the specified ID"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> (find-entity-by-id s id)
         after-read)))

(defn find-by-name
  "Finds the entity having the specified name
  for the specified user"
  [storage-spec user entity-name]
  (->> (select storage-spec (:id user))
       (filter #(= entity-name (:name %)))
       first))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [storage-spec user entity-name]
  (or
    (find-by-name storage-spec user entity-name)
    (create storage-spec {:user-id (:id user)
                          :name entity-name})))

(def update
  (update-fn {:update update-entity
              :spec ::existing-entity
              :coercion-rules coercion-rules
              :rule-fn validation-rules
              :before-save before-save
              :after-read after-read
              :find find-by-id}))

(defn delete
  "Removes the specifiedy entity and all related records from storage"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-entity s id)))
