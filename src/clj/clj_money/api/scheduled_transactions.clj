(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [clj-time.core :as t]
            [stowaway.core :as storage]
            [clj-money.util :refer [update-in-if
                                    unserialize-date]]
            [clj-money.api :refer [->response
                                   creation-response
                                   not-found]]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             +scope]
             :as authorization]
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
  (->response
    (sched-trans/search (->criteria req))))

(defn- ->sched-trans-item
  [item]
  (-> item
      (update-in-if [:quantity] bigdec)
      (update-in-if [:action] keyword)))

(defn- ->sched-tran
  [body]
  (-> body
      (update-in-if [:items] #(map ->sched-trans-item %))
      (update-in-if [:interval-type] keyword)
      (update-in-if [:start-date] unserialize-date)
      (update-in-if [:end-date] unserialize-date)
      (update-in-if [:last-occurrence] unserialize-date)
      (storage/tag ::models/scheduled-transaction)))

(defn- create
  [{:keys [params authenticated body]}]
  (-> body
      ->sched-tran
      (assoc :entity-id (:entity-id params))
      (authorize ::authorization/create authenticated)
      sched-trans/create
      creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (when-let [sched-tran (sched-trans/find (:id params))]
    (authorize sched-tran
               action
               authenticated)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [sched-tran (find-and-authorize req ::authorization/update)]
    (-> sched-tran
        (merge (->sched-tran body))
        sched-trans/update
        ->response)
    (not-found)))

(defn- delete
  [req]
  (if-let [sched-tran (find-and-authorize req ::authorization/destroy)]
    (do
      (sched-trans/delete sched-tran)
      (->response))
    (not-found)))

(defn- realize
  [req]
  (if-let [sched-tran (find-and-authorize req ::sched-trans-auth/realize)]
    (-> sched-tran
        sched-trans/realize
        creation-response)
    (not-found)))

(defn- mass-realize
  [{:keys [params authenticated]}]
  (if-let [entity (entities/find-by (+scope {:id (:entity-id params)}
                                            ::models/entity
                                            authenticated))]
    (->> (sched-trans/search {:entity-id (:id entity)
                              :enabled true})
         (filter #(and (or (nil? (:end-date %))
                           (t/before? (t/today) (:end-date %)))
                       (allowed? % ::sched-trans-auth/realize authenticated)))
         (mapcat sched-trans/realize)
         (sort-by :transaction-date t/before?)
         creation-response)
    (not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/scheduled-transactions" req (index req))
  (POST "/api/entities/:entity-id/scheduled-transactions" req (create req))
  (POST "/api/entities/:entity-id/scheduled-transactions/realize" req (mass-realize req))
  (PATCH "/api/scheduled-transactions/:id" req (update req))
  (DELETE "/api/scheduled-transactions/:id" req (delete req))
  (POST "/api/scheduled-transactions/:id/realize" req (realize req)))
