(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH]]
            [stowaway.core :as stow]
            [clj-money.util :refer [uuid
                                    unserialize-date
                                    parse-int
                                    update-in-if]]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.validation :as v]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
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
  (->response
    (recs/search (env :db)
                 (extract-criteria req)
                 (extract-options req))))

(defn- unserialize-item-ref
  [item-ref]
  (-> item-ref
      (update-in [0] uuid)
      (update-in [1] unserialize-date)))

(defn- create
  [{:keys [params body authenticated]}]
  (let [result (as-> params r
                 (select-keys r [:account-id])
                 (merge r (select-keys body [:end-of-period
                                             :balance
                                             :status
                                             :item-refs]))
                 (update-in r [:status] keyword)
                 (update-in r [:balance] bigdec)
                 (update-in r [:end-of-period] unserialize-date)
                 (update-in r [:item-refs] #(map unserialize-item-ref %))
                 (stow/tag r ::models/reconciliation)
                 (authorize r ::auth/create authenticated)
                 (recs/create (env :db) r))]
    (->response result
                (if (v/has-error? result)
                  400
                  201))))

(defn- extract-recon
  [{:keys [body]}]
  (-> body
      (dissoc :id)
      (update-in-if [:status] keyword)
      (update-in-if [:balance ] bigdec)
      (update-in-if [:end-of-period] unserialize-date)
      (update-in-if [:item-refs] (fn [item-refs]
                                   (map #(-> %
                                             (update-in [0] uuid)
                                             (update-in [1] unserialize-date))
                                        item-refs)))))

(defn- scoped-find
  [{:keys [params authenticated]}]
  (first (recs/search (env :db)
                      (+scope {:id (uuid (:id params))}
                              ::models/reconciliation
                              authenticated)
                      {:limit 1})))

(defn- update
  [{:keys [authenticated] :as req}]
  (if-let [recon (scoped-find req)]
    (do
      (authorize recon ::auth/update authenticated)
      (->response (recs/update (env :db)
                               (merge recon
                                      (extract-recon req)))))
    (not-found)))

(defroutes routes
  (GET "/api/accounts/:account-id/reconciliations" req (index req))
  (POST "/api/accounts/:account-id/reconciliations" req (create req))
  (PATCH "/api/reconciliations/:id" req (update req)))
