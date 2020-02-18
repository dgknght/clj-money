(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET DELETE POST PATCH]]
            [clj-money.authorization :refer [apply-scope
                                             authorize
                                             tag-resource]]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.x-platform.util :refer [unserialize-date]]
            [clj-money.models.prices :as prices]
            [clj-money.permissions.prices])
  (:import java.util.UUID))

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
  (->response (prices/search (env :db) (->  params
                                           (handle-start-end-dates)
                                           (select-keys [:commodity-id :entity-id :trade-date])
                                           (apply-scope :price authenticated)))))

(defn- create
  [{:keys [params body authenticated]}]
  (if-let [price (-> params
                     (merge body)
                     (select-keys [:commodity-id :trade-date :price])
                     (update-in [:trade-date] unserialize-date)
                     (tag-resource :price)
                     (authorize :create authenticated))]
    (->response (prices/create (env :db) price)
                201)
    (not-found)))

(defn- update
  [{:keys [params authenticated body]}]
  (if-let [price (authorize (prices/find-by-id (env :db)
                                               (UUID/fromString (:id params))
                                               (unserialize-date (:trade-date params)))
                            :update
                            authenticated)]
    (->response (prices/update (env :db) (merge price (-> body
                                                          (select-keys [:price :trade-date])
                                                          (update-in [:trade-date] unserialize-date)))))
    (not-found)))

(defn- delete
  [{:keys [params authenticated]}]
  (if-let [price (authorize (prices/find-by-id (env :db)
                                               (UUID/fromString (:id params))
                                               (unserialize-date (:trade-date params)))
                            :delete
                            authenticated)]
    (do
      (prices/delete (env :db) price)
      (->response))
    (not-found)))

(defroutes routes
  (POST "/api/commodities/:commodity-id/prices" req (create req))
  (GET "/api/commodities/:commodity-id/prices" req (index req))
  (PATCH "/api/prices/:trade-date/:id" req (update req))
  (DELETE "/api/prices/:trade-date/:id" req (delete req)))
