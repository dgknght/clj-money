(ns clj-money.api.lot-notes
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.entities :as entities]
            [clj-money.accounts :as acts]
            [clj-money.authorization.lot-notes]))

(defn- extract-criteria
  [{{:keys [account-id]} :params}]
  (let [account (entities/find account-id)]
    (cond
      (acts/trading? account) {:lot/account account}
      (acts/tradable? account) {:lot/account (:account/parent account)})))

(defn- index
  [{:keys [authenticated] :as req}]
  (or (some-> (extract-criteria req)
              (+scope :lot-note authenticated)
              entities/select
              api/response)
      api/not-found))

(defn- extract-note
  [{{:keys [commodity-id]} :params :keys [body-params]}]
  (let [commodity (entities/find commodity-id)
        lots (entities/select {:lot/commodity commodity
                               :lot/shares-owned [:!= 0M]})]
    (-> body-params
        (select-keys [:lot-note/transaction-date
                      :lot-note/memo])
        (assoc :lot-note/lots lots))))

(defn- create
  [{:keys [authenticated] :as req}]
  (or (some-> (extract-note req)
              (authorize ::auth/create authenticated)
              entities/put
              api/creation-response)
      api/not-found))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [note (entities/find-by
                    (-> params
                        (select-keys [:id])
                        (+scope :lot-note authenticated)))]
    (authorize note action authenticated)))

(defn- delete
  [req]
  (if-let [note (find-and-auth req ::auth/destroy)]
    (do
      (entities/delete note)
      (api/response))
    api/not-found))

(def routes
  [["accounts/:account-id/lot-notes" {:get {:handler index}}]
   ["commodities/:commodity-id/lot-notes" {:post {:handler create}}]
   ["lot-notes/:id" {:delete {:handler delete}}]])
