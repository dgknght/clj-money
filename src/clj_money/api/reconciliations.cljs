(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.dates :refer [nominal-comparatives]]
            [clj-money.api :as api :refer [add-error-handler]]))


(defn select
  [{:reconciliation/keys [account] :as criteria} & {:as opts}]
  {:pre [account]}
  (let [prepared-criteria (-> criteria
                              (update-in-if [:reconciliation/status] name)
                              (nominal-comparatives :reconciliation/end-of-period)
                              (dissoc :reconciliation/account))]
    (api/get (api/path :accounts
                       account
                       :reconciliations)
             prepared-criteria
             (add-error-handler opts "Unable to retrieve the reconciliations: %s"))))

(defn create
  [{:keys [account-id] :as reconciliation} opts]
  (api/post (api/path :accounts
                      account-id
                      :reconciliations)
            (dissoc reconciliation :account-id)
            (add-error-handler opts "Unable to create the reconciliation: %s")))

(defn update
  [{:keys [id] :as reconciliation} opts]
  (api/patch (api/path :reconciliations
                       id)
             (dissoc reconciliation :id :account-id)
             (add-error-handler opts "Unable to update the reconciliation: %s")))

(defn save
  [reconciliation & {:as opts}]
  (let [f (if (:id reconciliation)
            update
            create)]
    (f reconciliation opts)))
