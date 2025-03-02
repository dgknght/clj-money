(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.test-assertions]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             +scope]
             :as authorization]
            [clj-money.db :as db]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.scheduled-transactions :as sched-trans]
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

(defn- extract-sched-tran
  [{:keys [params]}]
  (select-keys params [:scheduled-transaction/description
                       :scheduled-transaction/start-date
                       :scheduled-transaction/date-spec
                       :scheduled-transaction/interval-type
                       :scheduled-transaction/interval-count
                       :scheduled-transaction/memo
                       :scheduled-transaction/items]))

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
  [req]
  (or (some-> (find-and-authorize req ::authorization/update)
              (merge (extract-sched-tran req))
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
  (or (some-> (find-and-authorize req ::sched-trans-auth/realize)
              sched-trans/realize
              models/put-many
              api/creation-response)
      api/not-found))

(defn- mass-realize
  [{:keys [params authenticated]}]
  (if-let [entity (models/find-by (+scope {:id (:entity-id params)}
                                          :entity
                                          authenticated))]
    (->> (models/select
           [:and
            #:scheduled-transaction{:entity entity
                                    :enabled true}
            [:or
             {:scheduled-transaction/end-date nil}
             {:scheduled-transaction/end-date [:< (t/local-date)]}]])
         (filter #(allowed? % ::sched-trans-auth/realize authenticated))
         (mapcat sched-trans/realize)
         (sort-by :transaction/transaction-date t/before?)
         models/put-many
         (filter (db/model-type? :transaction))
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
