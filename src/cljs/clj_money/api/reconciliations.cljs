(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [find update])
  (:require [clj-money.api :as api]
            [clj-money.util :refer [serialize-date
                                    unserialize-date]]))

(defn- after-read
  [reconciliation]
  (update-in reconciliation [:end-of-period] unserialize-date))

(defn search
  [{:keys [account-id] :as criteria} success-fn error-fn]
  {:pre [account-id]}

  (api/get-resources (api/path :accounts
                               account-id
                               :reconciliations)
                     (dissoc criteria account-id)
                     (comp success-fn #(map after-read %))
                     error-fn))

(defn find
  [criteria success-fn error-fn]
  (search (assoc criteria :limit 1)
          (comp success-fn first)
          error-fn))

(defn- serialize-item-ref
  [item-ref]
  (update-in item-ref [1] serialize-date))

(defn- before-save
  [reconciliation]
  (-> reconciliation
      (update-in [:end-of-period] serialize-date)
      (update-in [:item-refs] #(map serialize-item-ref %))))

(defn create
  [{:keys [account-id] :as reconciliation} success-fn error-fn]
  (api/create-resource
   (api/path :accounts
             account-id
             :reconciliations)
   (-> reconciliation
       (dissoc :account-id)
       before-save)
   success-fn
   error-fn))

(defn update
  [{:keys [id] :as reconciliation} success-fn error-fn]
  (api/update-resource
   (api/path :reconciliations
             id)
   (-> reconciliation
       (dissoc :id :account-id)
       before-save)
   success-fn
   error-fn))

(defn save
  [reconciliation success-fn error-fn]
  (if (:id reconciliation)
    (update reconciliation success-fn error-fn)
    (create reconciliation success-fn error-fn)))
