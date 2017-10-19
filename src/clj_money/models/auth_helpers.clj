(ns clj-money.models.auth-helpers
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-money.authorization :as authorization]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]))

(defmulti ^:private lookup-entity-id
  (fn [_ resource]
    (authorization/get-resource-tag resource)))

(defmethod ^:private lookup-entity-id :entity
  [_ resource]
  (:id resource))

(defmethod ^:private lookup-entity-id :account
  [_ resource]
  (:entity-id resource))

(defmethod ^:private lookup-entity-id :transaction
  [_ resource]
  (:entity-id resource))

(defn user-entity-ids
  ([user context]
   (user-entity-ids user context {}))
  ([user context options]
   (->> (entities/select (:storage-spec context) (:id user) options)
        (map :id)
        (into #{}))))

(defn all-user-entity-ids
  [user context]
  (user-entity-ids user context {:include-grants? true}))

(defn user-owns-entity?
  [user resource & args]
  (let [context (last args)]
    (contains? (user-entity-ids user context)
               (:entity-id resource))))

(defn user-granted-access?
  [user resource action {storage-spec :storage-spec :as context}]
  (let [resource-type (authorization/get-resource-tag resource)
        entity-id (lookup-entity-id context resource)
        grant (-> (grants/search storage-spec
                                 {:user-id (:id user)
                                  :entity-id entity-id}
                                 {:limit 1})
                  first)]
    (grants/has-permission? grant resource-type action)))

(authorization/->context :storage-spec (env :db))
