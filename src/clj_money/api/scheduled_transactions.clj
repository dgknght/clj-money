(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             +scope]
             :as authorization]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.scheduled-transactions :as sched-trans]
            [clj-money.authorization.scheduled-transactions :as sched-trans-auth]))

(defn- ->criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (+scope :scheduled-transaction authenticated)))

(defn- index
  [req]
  (-> req
      ->criteria
      models/select
      api/response))

(defn- ->sched-trans-item
  [item]
  (-> item
      (update-in-if [:scheduled-transaction-item/quantity] bigdec)
      (update-in-if [:scheduled-transaction-item/action] keyword)))

(defn- extract-sched-tran
  [{:keys [body]}]
  (-> body
      (update-in-if [:scheduled-transaction/items] #(map ->sched-trans-item %))
      (update-in-if [:scheduled-transaction/interval-type] keyword)
      (update-in-if [:scheduled-transaction/start-date] dates/unserialize-local-date)
      (update-in-if [:scheduled-transaction/end-date] dates/unserialize-local-date)
      (update-in-if [:scheduled-transaction/last-occurrence] dates/unserialize-local-date)))

(defn- create
  [{:keys [params authenticated] :as req}]
  (-> req
      extract-sched-tran
      (assoc :scheduled-transaction/entity {:id (:entity-id params)})
      (authorize ::authorization/create authenticated)
      models/put
      api/creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (models/find-by (+scope (select-keys params [:id])
                                     :scheduled-transaction
                                     authenticated))
             action
             authenticated))

(defn- update
  [{:keys [body] :as req}]
  (if-let [sched-tran (find-and-authorize req ::authorization/update)]
    (-> sched-tran
        (merge (extract-sched-tran body))
        models/put
        api/response)
    api/not-found))

(defn- delete
  [req]
  (if-let [sched-tran (find-and-authorize req ::authorization/destroy)]
    (do
      (models/delete sched-tran)
      (api/response))
    api/not-found))

(defn- realize
  [req]
  (if-let [sched-tran (find-and-authorize req ::sched-trans-auth/realize)]
    (-> sched-tran
        sched-trans/realize
        api/creation-response)
    api/not-found))

(defn- mass-realize
  [{:keys [params authenticated]}]
  (if-let [entity (models/find-by (+scope {:id (:entity-id params)}
                                            :entity
                                            authenticated))]
    (->> (models/select :#scheduled-transaction{:entity entity
                                                :enabled true})
         (filter #(and (or (nil? (:scheduled-transaction/end-date %))
                           (t/before? (t/local-date) (:scheduled-transaction/end-date %)))
                       (allowed? % ::sched-trans-auth/realize authenticated)))
         (mapcat sched-trans/realize)
         (sort-by :transaction/transaction-date t/before?)
         api/creation-response)
    api/not-found))

(def routes
  [["entities/:entity-id/scheduled-transactions"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/realize" {:post {:handler mass-realize}}]]
   ["scheduled-transactions/:id"
    ["" {:patch {:handler update}
         :delete {:handler delete}}]
    ["/realize" {:post {:handler realize}}]]])
