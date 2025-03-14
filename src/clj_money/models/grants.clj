(ns clj-money.models.grants
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :refer [keywordize-keys]]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :refer [with-validation]]
            [clj-money.models :as models]))

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::user-id integer?)
(s/def ::permissions (s/map-of keyword? set?))
(s/def ::new-grant (s/keys :req-un [::entity-id ::user-id ::permissions]))
(s/def ::existing-grant (s/keys :req-un [::id ::entity-id ::user-id ::permissions]))

(def resource-types
  #{:account
    :entity
    :transaction
    :budget
    :budget-item
    :commodity
    :price
    :attachment
    :reconciliation
    :report})

(def actions
  #{:show :create :update :delete})

(def available-permissions
  (->> {:report #{:income-statement :balance-sheet :budget}}
       (merge (reduce #(assoc %1 %2 actions) {} resource-types))))

(defn- before-save
  [grant]
  (tag grant ::models/grant))

(defn- prepare-permissions
  [permissions]
  (reduce (fn [p k]
            (update-in-if p [k] #(set (map keyword %))))
          (-> permissions
              keywordize-keys)
          [:account :transaction]))

(defn- after-read
  [grant]
  (when grant
    (-> grant
        (update-in [:permissions] prepare-permissions)
        (tag ::models/grant))))

(defn create
  [grant]
  (with-storage (env :db)
    (with-validation grant ::new-grant
      (-> grant
          before-save
          storage/create
          after-read))))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read (storage/select (tag criteria ::models/grant)
                                     options)))))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  [grant-or-id]
  (find-by {:id (->id grant-or-id)}))

(defn update
  [grant]
  (with-storage (env :db)
    (with-validation grant ::existing-grant
      (-> grant
          before-save
          storage/update)
      (find grant))))

(defn delete
  [grant]
  (with-storage (env :db)
    (storage/delete grant)))

(defn has-permission?
  [grant resource-type action]
  (when grant ; TODO: change this not to tolerate nil values
    (action
     (->> grant
          :permissions
          resource-type))))
