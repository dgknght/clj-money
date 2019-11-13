(ns clj-money.models.auth-helpers
  (:require
            [environ.core :refer [env]]
            [clj-money.authorization :as authorization]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.grants :as grants]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.commodities :as commodities]))

(defmulti ^:private lookup-entity-id
  (fn [_ resource]
    (authorization/get-resource-tag resource)))

(defmethod ^:private lookup-entity-id :entity
  [_ resource]
  (:id resource))

(defmethod ^:private lookup-entity-id :default
  [_ resource]
  (or (:entity-id resource)
      (throw (ex-info "No :entity-id value present" {:resource resource
                                                     :meta (meta resource)}))))

(defmethod ^:private lookup-entity-id :attachment
  [{storage-spec :storage-spec :as context}
   {:keys [transaction-id transaction-date]}]
  (lookup-entity-id
    context
    (transactions/find-by-id storage-spec
                             transaction-id
                             transaction-date)))

(defmethod ^:private lookup-entity-id :budget-item
  [{storage-spec :storage-spec :as context} resource]
  (->> (:budget-id resource)
       (budgets/find-by-id storage-spec)
       (lookup-entity-id context)))

(defmethod ^:private lookup-entity-id :price
  [{storage-spec :storage-spec :as context} resource]
  (->> (:commodity-id resource)
       (commodities/find-by-id storage-spec)
       (lookup-entity-id context)))

(defmethod ^:private lookup-entity-id :reconciliation
  [{storage-spec :storage-spec :as context} resource]
  (->> (:account-id resource)
       (accounts/find-by-id storage-spec)
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
    (= (:user-id entity) (:id user))))

(defn find-grant
  [user resource {storage-spec :storage-spec :as context}]
  (let [entity-id (lookup-entity-id context resource)]
    (-> (grants/search storage-spec
                       {:user-id (:id user)
                        :entity-id entity-id}
                       {:limit 1})
        first)))

(defn user-granted-access?
  [user resource action context]
  (grants/has-permission? (find-grant user resource context)
                          (authorization/get-resource-tag resource)
                          action))

(authorization/->context :storage-spec (env :db))
