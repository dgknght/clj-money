(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [find update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.dates :refer [nominal-comparatives]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [add-error-handler]]))


(defn search
  [{:keys [account-id] :as criteria} & {:as opts}]
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
             (add-error-handler opts "Unable to retrieve the reconciliations: %s"))))

(defn find
  [criteria & {:as opts}]
  (apply search
         (assoc criteria :limit 1)
         (mapcat identity opts)))

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
