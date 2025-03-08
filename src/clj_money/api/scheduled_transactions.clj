(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [java-time.api :as t]
            [stowaway.core :as storage]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.authorization :refer [authorize
                                                   allowed?
                                                   +scope]
             :as authorization]
            [clj-money.models :as models]
            [clj-money.models.scheduled-transactions :as sched-trans]
            [clj-money.models.entities :as entities]
            [clj-money.authorization.scheduled-transactions :as sched-trans-auth]))

(defn- ->criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (+scope ::models/scheduled-transaction authenticated)))

(defn- index
  [req]
  (api/response
    (sched-trans/search (->criteria req))))

(defn- extract-sched-tran
  [{:keys [params]}]
  (storage/tag params ::models/scheduled-transaction))

(defn- create
  [{:keys [params authenticated] :as req}]
  (-> req
      extract-sched-tran
      (assoc :entity-id (:entity-id params))
      (authorize ::authorization/create authenticated)
      sched-trans/create
      api/creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (when-let [sched-tran (sched-trans/find (:id params))]
    (authorize sched-tran
               action
               authenticated)))

(defn- update
  [req]
  (if-let [sched-tran (find-and-authorize req ::authorization/update)]
    (-> sched-tran
        (merge (extract-sched-tran req))
        sched-trans/update
        api/response)
    api/not-found))

(defn- delete
  [req]
  (if-let [sched-tran (find-and-authorize req ::authorization/destroy)]
    (do
      (sched-trans/delete sched-tran)
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
  (if-let [entity (entities/find-by (+scope {:id (:entity-id params)}
                                            ::models/entity
                                            authenticated))]
    (->> (sched-trans/search {:entity-id (:id entity)
                              :enabled true})
         (filter #(and (or (nil? (:end-date %))
                           (t/before? (t/local-date) (:end-date %)))
                       (allowed? % ::sched-trans-auth/realize authenticated)))
         (mapcat sched-trans/realize)
         (sort-by :transaction-date t/before?)
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
