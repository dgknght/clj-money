(ns clj-money.models.auth-helpers
  (:require [environ.core :refer [env]]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.grants :as grants]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.commodities :as commodities]))

(defmulti ^:private lookup-entity-id
  (fn [resource]
    (models/tag resource)))

(defmethod ^:private lookup-entity-id :default
  [resource]
  (or (:entity-id resource)
      (throw (ex-info "No :entity-id value present" {:resource resource
                                                     :meta (meta resource)}))))

(defmethod ^:private lookup-entity-id ::models/entity
  [resource]
  (:id resource))

(defmethod ^:private lookup-entity-id ::models/attachment
  [{:keys [transaction-id transaction-date]}]
  (lookup-entity-id
    (transactions/find-by-id (env :db)
                             transaction-id
                             transaction-date)))

(defmethod ^:private lookup-entity-id ::models/budget-item
  [{:keys [budget-id]}]
  (lookup-entity-id
    (budgets/find-by-id (env :db) budget-id)))

(defmethod ^:private lookup-entity-id ::models/price
  [{:keys [commodity-id]}]
  (lookup-entity-id
    (commodities/find-by-id (env :db) commodity-id)))

(defmethod ^:private lookup-entity-id ::models/reconciliation
  [{storage-spec :storage-spec :as context} resource]
  (->> (:account-id resource)
       (accounts/find-by-id storage-spec)
       (lookup-entity-id context)))

(defn- lookup-entity
  [resource]
  (if (entities/entity? resource)
    resource
    (entities/find-by-id (env :db) (lookup-entity-id resource))))

(defn user-entity-ids
  ([user context]
   (user-entity-ids user context {}))
  ([user context options]
   (->> (entities/select (:storage-spec context) {:user-id (:id user)} options)
        (map :id)
        (into #{}))))

(defn all-user-entity-ids
  [user context]
  (user-entity-ids user context {:include-grants? true}))

(defn user-owns-entity?
  [resource user]
  (= (:user-id (lookup-entity resource))
     (:id user)))

(defn find-grant
  [user resource]
  (let [entity-id (lookup-entity-id resource)]
    (-> (grants/search (env :db)
                       {:user-id (:id user)
                        :entity-id entity-id}
                       {:limit 1})
        first)))

(defn user-granted-access?
  [resource user action]
  (when-let [g (find-grant user resource)]
    (grants/has-permission? g
                            (models/tag resource)
                            action)))

(defn owner-or-granted?
  [resource user action]
  (or (user-owns-entity? resource user)
      (user-granted-access? resource user action)))
