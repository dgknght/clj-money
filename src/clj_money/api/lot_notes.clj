(ns clj-money.api.lot-notes
  (:require [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization.lot-notes]))

(defn- index
  [{:keys [authenticated]}]
  (api/response
    (entities/select
      (+scope {} :lot-note authenticated))))

(defn- extract-note
  [{{:keys [lot-id]} :params :keys [body-params]}]
  (-> body-params
      (select-keys [:lot-note/transaction-date
                    :lot-note/memo])
      (assoc :lot-note/lots [(util/->entity-ref lot-id)])))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> (extract-note req)
      (authorize ::auth/create authenticated)
      entities/put
      api/creation-response))

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
  [["lots/:lot-id/lot-notes" {:get {:handler index}
                               :post {:handler create}}]
   ["lot-notes/:id" {:delete {:handler delete}}]])
