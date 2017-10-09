(ns clj-money.models.grants
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-entity-ids
                                                   user-owns-entity?]]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-grant
                                              select-grants
                                              update-grant
                                              delete-grant]]
            [clj-money.models.entities :as entities]))

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::user-id integer?)
(s/def ::permissions #(not (nil? %)))
(s/def ::new-grant (s/keys :req-un [::entity-id ::user-id ::permissions]))
(s/def ::existing-grant (s/keys :req-un [::id ::entity-id ::user-id ::permissions]))

(def resource-types
  #{:account :transaction :budget :budget-item :commodity :price})

(def actions
  #{:index :show :new :create :edit :update :delete})

(defn- before-save
  [_ grant]
  (update-in grant [:permissions] prn-str))

(defn- after-read
  ([_ grant] (after-read grant))
  ([grant]
   (when grant
     (-> grant
         (update-in [:permissions] read-string)
         (authorization/tag-resource :grant)))))

(def ^:private coercion-rules
  [(coercion/rule :integer [:id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :integer [:user-id])])

(def create
  (create-fn {:create create-grant
              :spec ::new-grant
              :coercion-rules coercion-rules
              :before-save before-save
              :after-read after-read}))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (map after-read (select-grants s criteria))))

(defn find-by-id
  [storage-spec id]
  (first (search storage-spec {:id id})))

(def update
  (update-fn {:update update-grant
              :spec ::existing-grant
              :coercion-rules coercion-rules
              :before-save before-save
              :after-read after-read
              :find find-by-id}))

(defn delete
  [storage-spec grant]
  (with-storage [s storage-spec]
    (delete-grant s (:id grant))))

(defn has-permission
  [grant resource-type action]
  ((->> grant
        :permissions
        resource-type
        (into #{}))
   action))

(authorization/allow :grant [:new :create :show :edit :update :delete]
                     user-owns-entity?)

(authorization/set-scope
  :grant
  {:entity-id user-entity-ids})
