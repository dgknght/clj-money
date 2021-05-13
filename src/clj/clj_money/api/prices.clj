(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET DELETE POST PATCH]]
            [stowaway.core :as storage]
            [dgknght.app-lib.core :refer [update-in-if
                                          uuid
                                          parse-decimal]]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [dgknght.app-lib.authorization :refer [+scope
                                             authorize]
             :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
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
  (api/response
    (prices/search (->  params
                       (handle-start-end-dates)
                       (select-keys [:commodity-id :entity-id :trade-date])
                       (+scope ::models/price authenticated))
                   {:sort [[:trade-date :desc]]})))

(defn- extract-price
  [{:keys [params body]}]
  (-> params
      (merge body)
      (select-keys [:commodity-id :trade-date :price])
      (update-in-if [:trade-date] unserialize-date)
      (update-in-if [:price] parse-decimal)
      (storage/tag ::models/price)))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> req
      extract-price
      (authorize ::authorization/create authenticated)
      prices/create
      api/creation-response))

(defn- scoped-find
  [{:keys [params authenticated]} action]
  (authorize (prices/find (uuid (:id params))
                          (unserialize-date (:trade-date params)))
             action
             authenticated))

(defn- update
  [{:keys [body] :as req}]
  (if-let [price (scoped-find req ::authorization/update)]
    (api/response
      (prices/update
        (merge price (-> body
                         (select-keys [:price :trade-date])
                         (update-in-if [:price] bigdec)
                         (update-in [:trade-date] unserialize-date)))))
    api/not-found))

(defn- delete
  [req]
  (if-let [price (scoped-find req ::authorization/destroy)]
    (do
      (prices/delete price)
      (api/response))
    api/not-found))

(defroutes routes
  (POST "/api/commodities/:commodity-id/prices" req (create req))
  (GET "/api/prices" req (index req))
  (PATCH "/api/prices/:trade-date/:id" req (update req))
  (DELETE "/api/prices/:trade-date/:id" req (delete req)))
