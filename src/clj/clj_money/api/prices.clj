(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET DELETE POST PATCH]]
            [stowaway.core :as storage]
            [clj-money.models :as models]
            [clj-money.authorization :refer [+scope
                                             authorize]
             :as authorization]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.util :refer [update-in-if
                                    uuid
                                    unserialize-date]]
            [clj-money.models.prices :as prices]
            [clj-money.authorization.prices]))

(defn- handle-start-end-dates
  [params]
  (let [[start-date end-date]  (->> [:start-date :end-date]
                                    (map #(get-in params [%]))
                                    (map unserialize-date))
        result (dissoc params :start-date :end-date)]
    (cond
      (and start-date end-date)
      (assoc result :trade-date [:between start-date end-date])

      start-date
      (assoc result :trade-date [:>= start-date])

      end-date
      (assoc result :trade-date [:<= end-date])

      :else
      result)))

(defn- index
  [{:keys [params authenticated]}]
  {:pre [(or (:trade-date params)
             (and (:start-date params)
                  (:end-date params)))]}
  (->response (prices/search (env :db)
                             (->  params
                                 (handle-start-end-dates)
                                 (select-keys [:commodity-id :entity-id :trade-date])
                                 (+scope ::models/price authenticated))
                             {:sort [[:trade-date :desc]]})))

(defn- create
  [{:keys [params body authenticated]}]
  (if-let [price (-> params
                     (merge body)
                     (select-keys [:commodity-id :trade-date :price])
                     (update-in [:trade-date] unserialize-date)
                     (storage/tag ::models/price)
                     (authorize ::authorization/create authenticated))]
    (->response (prices/create (env :db) price)
                201)
    (not-found)))

(defn- scoped-find
  [{:keys [params authenticated]} action]
  (authorize (prices/find-by-id (env :db)
                                (uuid (:id params))
                                (unserialize-date (:trade-date params)))
             action
             authenticated))

(defn- update
  [{:keys [body] :as req}]
  (if-let [price (scoped-find req ::authorization/update)]
    (->response (prices/update
                  (env :db)
                  (merge price (-> body
                                   (select-keys [:price :trade-date])
                                   (update-in-if [:price] bigdec)
                                   (update-in [:trade-date] unserialize-date)))))
    (not-found)))

(defn- delete
  [req]
  (if-let [price (scoped-find req ::authorization/destroy)]
    (do
      (prices/delete (env :db) price)
      (->response))
    (not-found)))

(defroutes routes
  (POST "/api/commodities/:commodity-id/prices" req (create req))
  (GET "/api/commodities/:commodity-id/prices" req (index req))
  (PATCH "/api/prices/:trade-date/:id" req (update req))
  (DELETE "/api/prices/:trade-date/:id" req (delete req)))
