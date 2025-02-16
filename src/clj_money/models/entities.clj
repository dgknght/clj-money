(ns clj-money.models.entities
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?]]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]))

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

(def ^:private find-or-create*
  (some-fn models/find-by models/put))

(defn find-or-create
  "Finds the entity with the specified name for the
  specified user, or creates it if it is not found."
  [user entity-name]
  (find-or-create* #:entity{:user user
                            :name entity-name}))
