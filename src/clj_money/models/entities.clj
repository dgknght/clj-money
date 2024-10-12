(ns clj-money.models.entities
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]
            [clj-money.db :as db]))

(declare find-by)

(defn- name-is-unique?
  [{:as entity :keys [id]}]
  (-> entity
      (select-keys [:entity/name :entity/user])
      (assoc-if :id (when id [:!= id]))
      models/find-by
      nil?))
(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:entity/name]})

(s/def :entity/name (s/and string?
                     present?))
(s/def :entity/user ::models/model-ref)
(s/def :settings/monitored-account-ids (s/coll-of integer? :kind set?))
(s/def :settings/inventory-method #{:fifo :lifo})
(s/def :settings/default-commodity ::models/model-ref)
(s/def :entity/settings (s/nilable
                          (s/keys :opt [:settings/inventory-method
                                        :settings/monitored-account-ids
                                        :settings/default-commodity])))

(s/def ::models/entity (s/and (s/keys :req [:entity/name
                                            :entity/user]
                                      :opt [:entity/settings])
                              name-is-unique?))

(defn ^:deprecated select
  "Returns entities for the specified user"
  ([criteria]
   (select criteria {}))
  ([criteria options]
   (db/select (db/storage)
              (db/model-type criteria :entity)
              options)))

(defn ^:deprecated find-by
  "Returns the first entity that matches the specified criteria"
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (models/select criteria (merge options {:limit 1})))))

(defn ^:deprecated find
  "Finds the entity with the specified ID"
  [id-or-entity]
  (models/find-by {:id (->id id-or-entity)}))

(def ^:private find-or-create*
  (some-fn models/find-by models/put))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [user entity-name]
  (find-or-create* #:entity{:user user
                            :name entity-name}))

(defn ^:deprecatedn delete
  "Removes the specifiedy entity and all related records from storage"
  [entity]
  (db/delete (db/storage) [entity]))

(defn entity?
  [model]
  (= :entity (db/model-type model)))
