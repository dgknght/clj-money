(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [find update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.dates :refer [nominal-comparatives]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]))

(defn- after-read
  [reconciliation]
  (update-in reconciliation [:end-of-period] unserialize-date))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn search
  [{:keys [account-id] :as criteria} xf]
  {:pre [account-id]}
  (let [prepared-criteria (-> criteria
                              (update-in-if [:status] name)
                              (update-in-if [:desc] name)
                              (nominal-comparatives :end-of-period)
                              (update-in-if [:end-of-period-or-after] serialize-date)
                              (update-in-if [:end-of-period-or-before] serialize-date)
                              (dissoc account-id))]
    (api/get (api/path :accounts
                       account-id
                       :reconciliations)
             prepared-criteria
             {:transform (transform xf)
              :handle-ex (handle-ex "Unable to retrieve the reconciliations: %s")})))

(defn find
  [criteria xf]
  (search (assoc criteria :limit 1)
          (comp (map first)
                xf)))

(defn- serialize-item-ref
  [item-ref]
  (update-in item-ref [1] serialize-date))

(defn- before-save
  [reconciliation]
  (-> reconciliation
      (update-in [:end-of-period] serialize-date)
      (update-in [:item-refs] #(map serialize-item-ref %))))

(defn create
  [{:keys [account-id] :as reconciliation} xf]
  (api/post (api/path :accounts
                      account-id
                      :reconciliations)
            (-> reconciliation
                (dissoc :account-id)
                before-save)
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the reconciliation: %s")}))

(defn update
  [{:keys [id] :as reconciliation} xf]
  (api/patch (api/path :reconciliations
                       id)
             (-> reconciliation
                 (dissoc :id :account-id)
                 before-save)
             {:transform (transform xf)
              :handle-ex (handle-ex "Unable to update the reconciliation: %s")}))

(defn save
  [reconciliation xf]
  (let [f (if (:id reconciliation)
            update
            create)]
    (f reconciliation xf)))
