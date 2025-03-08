(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-int
                                          uuid]]
            [dgknght.app-lib.dates :refer [symbolic-comparatives]]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [clj-money.authorization.reconciliations]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (symbolic-comparatives :end-of-period)
      (select-keys [:account-id :status :end-of-period])
      (rename-keys {:account-id :reconciliation/account
                    :status :reconciliation/status
                    :end-of-period :reconciliation/end-of-period})
      (+scope :reconciliation authenticated)))

(defn- translate-sort
  [{:keys [asc desc] :as opts}]
  (cond-> opts
    asc  (assoc :sort [[(keyword "reconciliation" asc) :asc]])
    desc (assoc :sort [[(keyword "reconciliation" desc) :desc]])))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      translate-sort
      (select-keys [:limit :sort])
      (update-in-if [:limit] parse-int)))

(defn- index
  [req]
  (api/response
    (models/select (extract-criteria req)
                   (extract-options req))))

(defn- extract-recon
  [{:keys [params]}]
  (select-keys params
               [:reconciliation/end-of-period
                :reconciliation/balance
                :reconciliation/status
                :reconciliation/item-refs]))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> req
      extract-recon
      (authorize ::auth/create authenticated)
      models/put
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (update-in [:id] uuid)
          (+scope :reconciliation authenticated)
          models/find-by
          (authorize action authenticated)))

(defn- update
  [req]
  (or (some-> req
              (find-and-auth ::auth/update)
              (merge (extract-recon req))
              models/put
              api/update-response)
      api/not-found))

(def routes
  [["accounts/:account-id/reconciliations" {:get {:handler index}
                                            :post {:handler create}}]
   ["reconciliations/:id" {:patch {:handler update}}]])
