(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.models.schema :as schema]
            [clj-money.dates :as dates]
            [clj-money.util :as util :refer [update-keys]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- nominal-comparatives
  [m]
  (let [[oper v1 v2] (get-in m [:reconciliation/end-of-period])]
    (cond-> (dissoc m :reconciliation/end-of-period)
      (= oper :between) (assoc :reconciliation/end-of-period-on-or-after v1
                               :reconciliation/end-of-period-on-or-before v2))))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (dissoc :reconciliation/account)
      (nominal-comparatives)
      (update-in-if [:reconciliation/status] name)
      (update-in-if [:reconciliation/end-of-period-on-or-before] dates/serialize-local-date)
      (update-in-if [:reconciliation/end-of-period-on-or-after] dates/serialize-local-date)
      (update-in-if [:desc] name)
      (update-keys util/url-safe-keyword)))

(defn select
  [{:reconciliation/keys [account] :as criteria} & {:as opts}]
  {:pre [(:reconciliation/account criteria)
         (:reconciliation/end-of-period criteria)]}
  (api/get (api/path :accounts
                     account
                     :reconciliations)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the reconciliations: %s")))

(defn- simplify-items
  [recon]
  (update-in-if recon
                [:reconciliation/items]
                (fn [items]
                  (map #(select-keys %
                                     [:id
                                      :transaction/transaction-date])
                       items))))

(defn create
  [{:reconciliation/keys [account] :as recon} opts]
  (api/post (api/path :accounts
                      account
                      :reconciliations)
            (-> recon
                simplify-items
                (dissoc recon :reconciliation/account))
            (add-error-handler opts "Unable to create the reconciliation: %s")))

(defn update
  [recon opts]
  (api/patch (api/path :reconciliations
                       recon)
             (-> recon
                 simplify-items
                 (dissoc :id :reconciliation/account))
             (add-error-handler opts "Unable to update the reconciliation: %s")))

(defn save
  [recon & {:as opts}]
  (let [f (if (:id recon)
            update
            create)]
    (f (schema/prune recon :reconciliation)
       opts)))
