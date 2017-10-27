(ns clj-money.models.auth-helpers
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-money.authorization :as authorization]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.models.transactions :as transactions]))

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

(defmethod ^:private lookup-entity-id :attachment
  [{storage-spec :storage-spec :as context} resource]
  (->> (:transaction-id resource)
       (transactions/find-by-id storage-spec)
       (lookup-entity-id context)))

(defn- lookup-entity
  [{storage-spec :storage-spec :as context} resource]
  (if (entities/entity? resource)
    resource
    (entities/find-by-id storage-spec (lookup-entity-id context resource))))

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
  (let [context (last args)
        entity (lookup-entity context resource)]
    (= (:user-id entity))))

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
