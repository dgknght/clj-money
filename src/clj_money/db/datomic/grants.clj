(ns clj-money.db.datomic.grants
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clj-money.entities :as models]
            [clj-money.db.datomic :as datomic]))

(defn- removed-permissions
  [before after]
  (when before
    (let [ids (-> before :grant/permissions meta :ids)]
      (->> (:grant/permissions before)
           (mapcat (fn [[k before-actions]]
                     (if-let [after-actions (get-in after [:grant/permissions k])]
                       (map (fn [action]
                              [:db/retract (ids k) :permission/actions action])
                            (set/difference before-actions after-actions))
                       [[:db/retract (:id before) :grant/permissions (ids k)]])))))))

(defmethod datomic/deconstruct :grant
  [grant]
  (cons grant
        (removed-permissions (models/before grant) grant)))

(defn- ->permission-entities
  [permissions]
  (mapv (fn [[k v]]
          #:permission{:scope k
                       :actions v})
        permissions))

(defmethod datomic/before-save :grant
  [grant]
  (update-in grant [:grant/permissions] ->permission-entities))

(defn- <-permission-entities
  [entities]
  (vary-meta
    (->> entities
         (map (juxt :permission/scope
                    (comp set :permission/actions)))
         (into {}))
    assoc
    :ids
    (->> entities
         (map (juxt :permission/scope :id))
         (into {}))))

(defmethod datomic/after-read :grant
  [grant]
  (update-in grant [:grant/permissions] <-permission-entities))
