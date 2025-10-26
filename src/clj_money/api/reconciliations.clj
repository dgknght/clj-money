(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.transaction-items :as itms]
            [clj-money.authorization.reconciliations]))

(defn- symbolic-comparatives
  [m]
  (let [lower (get-in m [:reconciliation/end-of-period-on-or-after])
        upper (get-in m [:reconciliation/end-of-period-on-or-before])]
    (cond-> (dissoc m
                    :reconciliation/end-of-period-on-or-before
                    :reconciliation/end-of-period-on-or-after)
      (and upper lower) (assoc :reconciliation/end-of-period [:between lower upper])
      upper (assoc :reconciliation/end-of-period [:<= upper])
      lower (assoc :reconciliation/end-of-period [:>= lower]))))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (update-keys util/<-url-safe-keyword)
      (update-in-if [:reconciliation/end-of-period-on-or-before] dates/unserialize-local-date)
      (update-in-if [:reconciliation/end-of-period-on-or-after] dates/unserialize-local-date)
      (symbolic-comparatives)
      (rename-keys {:account-id :reconciliation/account})
      (select-keys [:reconciliation/account
                    :reconciliation/status
                    :reconciliation/end-of-period])
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
      (update-in-if [:limit] parse-long)))

(defn- index
  [req]
  (api/response
    (entities/select (extract-criteria req)
                   (extract-options req))))

(defn- extract-recon
  [{:keys [params]}]
  (select-keys params
               [:reconciliation/end-of-period
                :reconciliation/balance
                :reconciliation/status
                :reconciliation/items]))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-recon
      (update-in-if [:reconciliation/items] itms/resolve-refs)
      (assoc :reconciliation/account {:id (:account-id params)})
      (authorize ::auth/create authenticated)
      entities/put
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :reconciliation authenticated)
          entities/find-by
          (authorize action authenticated)))

(defn- update
  [req]
  (or (some-> req
              (find-and-auth ::auth/update)
              (merge (extract-recon req))
              entities/put
              api/update-response)
      api/not-found))

(def routes
  [["accounts/:account-id/reconciliations" {:get {:handler index}
                                            :post {:handler create}}]
   ["reconciliations/:id" {:patch {:handler update}}]])
