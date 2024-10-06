(ns clj-money.models.entities
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.db :as db]))

(declare find-by)

(defn- name-is-unique?
  [{:as entity :keys [id]}]
  (-> entity
      (select-keys [:entity/name :entity/user])
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))
(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:entity/name]})

(s/def :entity/name (s/and string?
                     present?))
(s/def :entity/user map?) ; TODO: Make this more specific?
(s/def :settings/monitored-account-ids (s/coll-of integer? :kind set?))
(s/def :settings/inventory-method #{:fifo :lifo})
(s/def :settings/default-commodity-id integer?)
(s/def :entity/settings (s/nilable
                          (s/keys :opt [:settings/inventory-method
                                        :settings/monitored-account-ids
                                        :settings/default-commodity-id])))

(s/def ::entity (s/and (s/keys :req [:entity/name
                                     :entity/user]
                               :opt [:entity/settings])
                       name-is-unique?))

; (defn- after-read
;   [entity]
;   (when entity
;     (-> entity
;         (update-in-if [:settings] keywordize-keys)
;         (update-in-if [:settings :monitored-account-ids] set)
;         (update-in-if [:settings :inventory-method] keyword)
;         (update-in-if [:settings :earliest-transaction-date] t/local-date)
;         (update-in-if [:settings :latest-transaction-date] t/local-date)
;         (update-in-if [:settings :earliest-price-date] t/local-date)
;         (update-in-if [:settings :latest-price-date] t/local-date)
;         (tag ::models/entity))))

(defn select
  "Returns entities for the specified user"
  ([criteria]
   (select criteria {}))
  ([criteria options]
   (db/select (db/storage)
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

; (defn- before-save
;   [entity]
;   (-> entity
;       (tag ::models/entity)
;       (update-in-if [:settings :monitored-account-ids] set)
;       (update-in-if [:settings :earliest-transaction-date] #(t/format (t/formatter :iso-date) %))
;       (update-in-if [:settings :latest-transaction-date] #(t/format (t/formatter :iso-date) %))
;       (update-in-if [:settings :earliest-price-date] #(t/format (t/formatter :iso-date) %))
;       (update-in-if [:settings :latest-price-date] #(t/format (t/formatter :iso-date) %))))

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
  (with-ex-validation entity ::entity
    (let [records-or-ids (db/put (db/storage)
                                 [entity])]
      (resolve-put-result records-or-ids))))

(def ^:private find-or-create*
  (some-fn find-by put))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [user entity-name]
  (find-or-create* #:entity{:user user
                            :name entity-name}))

(defn delete
  "Removes the specifiedy entity and all related records from storage"
  [entity]
  (db/delete (db/storage) [entity]))

(defn entity?
  [model]
  (= :entity (db/model-type model)))
