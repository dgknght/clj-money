(ns clj-money.api.memo-ledger-entries
  (:require [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization.memo-ledger-entries]))

(defn- index
  [{:keys [authenticated] {:keys [lot-id]} :params}]
  (api/response
    (entities/select
      (+scope {:memo-ledger-entry/lot (util/->entity-ref lot-id)}
              :memo-ledger-entry
              authenticated))))

(defn- extract-entry
  [{{:keys [lot-id]} :params :keys [body-params]}]
  (-> body-params
      (select-keys [:memo-ledger-entry/transaction-date
                    :memo-ledger-entry/memo])
      (assoc :memo-ledger-entry/lot (util/->entity-ref lot-id))))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> (extract-entry req)
      (authorize ::auth/create authenticated)
      entities/put
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [entry (entities/find-by
                     (-> params
                         (select-keys [:id])
                         (+scope :memo-ledger-entry authenticated)))]
    (authorize entry action authenticated)))

(defn- delete
  [req]
  (if-let [entry (find-and-auth req ::auth/destroy)]
    (do
      (entities/delete entry)
      (api/response))
    api/not-found))

(def routes
  [["lots/:lot-id/memo-ledger-entries" {:get {:handler index}
                                        :post {:handler create}}]
   ["memo-ledger-entries/:id" {:delete {:handler delete}}]])
