(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET POST PATCH]]
            [stowaway.core :as stow]
            [dgknght.app-lib.core :refer [update-in-if
                                     parse-int
                                     uuid]]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [dgknght.app-lib.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [clj-money.models.reconciliations :as recs]
            [clj-money.authorization.reconciliations]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:account-id :status])
      (+scope ::models/reconciliation authenticated)))

(defn- translate-sort
  [{:keys [asc desc] :as opts}]
  (cond-> opts
    asc  (assoc :sort [[(keyword asc) :asc]])
    desc (assoc :sort [[(keyword desc) :desc]])))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      translate-sort
      (select-keys [:limit :sort])
      (update-in-if [:limit] parse-int)))

(defn- index
  [req]
  (api/response
   (recs/search (extract-criteria req)
                (extract-options req))))

(defn- unserialize-item-ref
  [item-ref]
  (-> item-ref
      (update-in [0] uuid)
      (update-in [1] unserialize-date)))

(defn- create
  [{:keys [params body authenticated]}]
  (-> params
      (select-keys [:account-id])
      (merge (select-keys body [:end-of-period
                                :balance
                                :status
                                :item-refs]))
      (update-in [:status] keyword)
      (update-in [:balance] bigdec)
      (update-in [:end-of-period] unserialize-date)
      (update-in [:item-refs] #(map unserialize-item-ref %))
      (stow/tag ::models/reconciliation)
      (authorize ::auth/create authenticated)
      recs/create
      api/creation-response))

(defn- extract-recon
  [{:keys [body]}]
  (-> body
      (dissoc :id)
      (update-in-if [:status] keyword)
      (update-in-if [:balance] bigdec)
      (update-in-if [:end-of-period] unserialize-date)
      (update-in-if [:item-refs] (fn [item-refs]
                                   (map #(-> %
                                             (update-in [0] uuid)
                                             (update-in [1] unserialize-date))
                                        item-refs)))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (update-in [:id] uuid)
          (+scope ::models/reconciliation authenticated)
          recs/find-by
          (authorize action authenticated)))

(defn- update
  [req]
  (if-let [recon (find-and-auth req ::auth/update)]
    (-> recon
        (merge (extract-recon req))
        recs/update
        api/update-response)
    api/not-found))

(defroutes routes
  (GET "/api/accounts/:account-id/reconciliations" req (index req))
  (POST "/api/accounts/:account-id/reconciliations" req (create req))
  (PATCH "/api/reconciliations/:id" req (update req)))
