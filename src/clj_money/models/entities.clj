(ns clj-money.models.entities
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]
            [dgknght.app-lib.core :refer [update-in-if
                                          assoc-if
                                          present?]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.db :as db]
            [clj-money.models :as models]))

(declare find-by)

(defn- name-is-unique?
  [{:keys [name id user-id]}]
  (-> {:name name
       :user-id user-id}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))
(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:name]})

(s/def ::name (s/and string?
                     present?))
(s/def ::id integer?)
(s/def ::user-id integer?)
(s/def ::monitored-account-ids (s/coll-of integer? :kind set?))
(s/def ::inventory-method #{:fifo :lifo})
(s/def ::default-commodity-id integer?)
(s/def ::settings (s/keys :opt-un [::inventory-method ::monitored-account-ids ::default-commodity-id]))
(s/def ::new-entity (s/and (s/keys :req-un [::name ::user-id] :opt-un [::settings])
                           name-is-unique?))
(s/def ::existing-entity (s/and (s/keys :req-un [::id ::name] :opt-un [::user-id ::settings])
                                name-is-unique?))

#_(defn- after-read
  [entity]
  (when entity
    (-> entity
        (update-in-if [:settings] keywordize-keys)
        (update-in-if [:settings :monitored-account-ids] set)
        (update-in-if [:settings :inventory-method] keyword)
        (update-in-if [:settings :earliest-transaction-date] t/local-date)
        (update-in-if [:settings :latest-transaction-date] t/local-date)
        (update-in-if [:settings :earliest-price-date] t/local-date)
        (update-in-if [:settings :latest-price-date] t/local-date)
        (tag ::models/entity))))

(defn select
  "Returns entities for the specified user"
  ([criteria]
   (select criteria {}))
  ([criteria options]
   (db/select  (db/storage)
              (db/model-type criteria :entity)
              options)))

(defn find-by
  "Returns the first entity that matches the specified criteria"
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (select criteria (merge options {:limit 1})))))

(defn find
  "Finds the entity with the specified ID"
  [id-or-entity]
  (find-by {:id (->id id-or-entity)}))

(defn- before-validation
  [entity]
  (update-in entity [:settings] (fnil identity {})))

#_(defn- before-save
  [entity]
  (-> entity
      (tag ::models/entity)
      (update-in-if [:settings :monitored-account-ids] set)
      (update-in-if [:settings :earliest-transaction-date] #(t/format (t/formatter :iso-date) %))
      (update-in-if [:settings :latest-transaction-date] #(t/format (t/formatter :iso-date) %))
      (update-in-if [:settings :earliest-price-date] #(t/format (t/formatter :iso-date) %))
      (update-in-if [:settings :latest-price-date] #(t/format (t/formatter :iso-date) %))))

(defn- yield-or-find
  [m-or-id]
  ; if we have a map, assume it's a model and return it
  ; if we don't, assume it's an ID and look it up
  (if (map? m-or-id)
    m-or-id
    (find m-or-id)))

(defn- resolve-put-result
  [records]
  (some yield-or-find records)) ; This is because when adding a user, identities are inserted first, so the primary record isn't the first one returned

(defn put
  [entity]
  (let [entity (before-validation entity)]
    (with-ex-validation entity ::new-entity
      (let [records-or-ids (db/put (db/storage)
                                   entity)]
        (resolve-put-result records-or-ids)))))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [user entity-name]
  (or
   (find-by {:user-id (:id user)
             :name entity-name})
   (create {:user-id (:id user)
            :name entity-name})))

(defn update
  [entity]
  (with-storage (env :db)
    (let [entity (before-validation entity)]
      (with-validation entity ::existing-entity
        (-> entity
            before-save
            storage/update)
        (find entity)))))

(defn delete
  "Removes the specifiedy entity and all related records from storage"
  [entity]
  (with-storage (env :db)
    (storage/delete entity)))

(defn entity?
  [model]
  (= ::models/entity (tag model)))
