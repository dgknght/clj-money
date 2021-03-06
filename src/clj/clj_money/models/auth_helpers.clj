(ns clj-money.models.auth-helpers
  (:require [stowaway.core :as storage]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.grants :as grants]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.attachments :as attachments]))

(defmulti ^:private lookup-entity-id
  (fn [resource]
    (storage/tag resource)))

(defmethod ^:private lookup-entity-id :default
  [resource]
  {:pre [(:entity-id resource)]}

  (:entity-id resource))

(defmethod ^:private lookup-entity-id ::models/entity
  [resource]
  (:id resource))

(defmethod ^:private lookup-entity-id ::models/attachment
  [{:keys [transaction-id transaction-date]}]
  (lookup-entity-id
   (transactions/find transaction-id
                      transaction-date)))

(defmethod ^:private lookup-entity-id ::models/budget-item
  [{:keys [budget-id]}]
  (lookup-entity-id
   (budgets/find budget-id)))

(defmethod ^:private lookup-entity-id ::models/price
  [{:keys [commodity-id]}]
  (lookup-entity-id
   (commodities/find commodity-id)))

(defmethod ^:private lookup-entity-id ::models/reconciliation
  [{:keys [account-id]}]
  (lookup-entity-id
   (accounts/find account-id)))

(defmethod ^:private lookup-entity-id ::models/image
  [{:keys [id]}]
  (lookup-entity-id
   (attachments/find-by {:image-id id})))

(defn- lookup-entity
  [resource]
  (if (entities/entity? resource)
    resource
    (entities/find (lookup-entity-id resource))))

(defn user-entity-ids
  ([user]
   (user-entity-ids user {}))
  ([user options]
   (->> (entities/select {:user-id (:id user)} options)
        (map :id)
        (into #{}))))

(defn all-user-entity-ids
  [user]
  (user-entity-ids user {:include-grants? true}))

(defn user-owns-entity?
  [resource user]
  (= (:user-id (lookup-entity resource))
     (:id user)))

(defn find-grant
  [user resource]
  (let [entity-id (lookup-entity-id resource)]
    (grants/find-by {:user-id (:id user)
                     :entity-id entity-id})))

(defn user-granted-access?
  [resource user action]
  (when-let [g (find-grant user resource)]
    (grants/has-permission? g
                            (storage/tag resource)
                            action)))

(defn owner-or-granted?
  [resource user action]
  (or (user-owns-entity? resource user)
      (user-granted-access? resource user action)))
