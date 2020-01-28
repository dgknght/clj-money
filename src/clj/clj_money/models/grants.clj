(ns clj-money.models.grants
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.util :refer [rev-args]]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-grant
                                              select-grants
                                              update-grant
                                              delete-grant]]))

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
  [grant & _]
  (update-in grant [:permissions] prn-str))

(defn- after-read
  [grant & _]
  (when grant
    (-> grant
        (update-in [:permissions] read-string)
        (authorization/tag-resource :grant))))

(def ^:private coercion-rules
  [(coercion/rule :integer [:id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :integer [:user-id])])

(def create
  (create-fn {:create (rev-args create-grant)
              :spec ::new-grant
              :coercion-rules coercion-rules
              :before-save before-save
              :after-read after-read}))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read (select-grants s criteria options)))))

(defn find-by-id
  [storage-spec id]
  (first (search storage-spec {:id id} {:limit 1})))

(def update
  (update-fn {:update (rev-args update-grant)
              :spec ::existing-grant
              :coercion-rules coercion-rules
              :before-save before-save
              :after-read after-read
              :find find-by-id}))

(defn delete
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-grant s id)))

(defn has-permission?
  [grant resource-type action]
  (when grant ; TODO: change this not to tolerate nil values
    (action
      (->> grant
          :permissions
          resource-type))))
