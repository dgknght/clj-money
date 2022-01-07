(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count])
  (:require [clj-time.core :as t]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as storage]
            [dgknght.app-lib.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [clj-money.models.commodities :as coms]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]
            [clj-money.authorization.commodities]))

(defn- scoped-params
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (+scope ::models/commodity authenticated)))

(defn- count
  [req]
  (api/response
    {:count (coms/count (scoped-params req))}))

(defn- append-current-prices
  [commodities]
  (if (seq commodities)
    (let [prices (prices/batch-fetch (set (map :id commodities))
                                     {:transform-fn identity
                                      :earliest-date (t/local-date 1900 1 1)})] ; TODO: get this from the entity
      (map #(assoc % :most-recent-price (get-in prices [(:id %)]))
           commodities))
    commodities))

(defn- append-shares-owned
  [commodities]
  (if (seq commodities)
    (let [lots (->> (lots/search {:commodity-id (map :id commodities)
                                  :shares-owned [:> 0]})
                    (group-by :commodity-id)
                    (map (fn [[commodity-id lots]]
                           [commodity-id (->> lots
                                              (map :shares-owned)
                                              (reduce + 0M))]))
                    (into {}))]
      (map #(assoc % :shares-owned (get-in lots [(:id %)] 0M))
           commodities))
    commodities))

(defn- index
  [req]
  (->> (coms/search (scoped-params req))
       append-current-prices
       append-shares-owned
       api/response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (coms/find (:id params))
             action
             authenticated))

(defn- show
  [req]
  (api/response
    (find-and-authorize req ::authorization/show)))

(def ^:private attribute-keys
  [:id
   :entity-id
   :name
   :symbol
   :exchange
   :type
   :price-config])

(defn- ensure-keyword
  [m & ks]
  (reduce #(if (contains? %1 %2)
             (update-in %1 [%2] keyword)
             m)
          m
          ks))

(defn- extract-commodity
  [{:keys [body params]}]
  (-> body
      (assoc :entity-id (:entity-id params))
      (select-keys attribute-keys)
      (ensure-keyword :exchange :type)
      (storage/tag ::models/commodity)))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> req
      extract-commodity
      (authorize ::authorization/create authenticated)
      coms/create
      api/creation-response))

(defn- update
  [{:keys [body] :as req}]
  (if-let [commodity (find-and-authorize req ::authorization/update)]
    (-> commodity
        (merge (-> body
                   (select-keys attribute-keys)
                   (ensure-keyword :exchange :type)))
        coms/update
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [commodity (find-and-authorize req ::authorization/destroy)]
    (do
      (coms/delete commodity)
      (api/response))
    api/not-found))

(defroutes routes
  (GET "/api/entities/:entity-id/commodities/count" req (count req))
  (GET "/api/entities/:entity-id/commodities" req (index req))
  (POST "/api/entities/:entity-id/commodities" req (create req))
  (GET "/api/commodities/:id" req (show req))
  (PATCH "/api/commodities/:id" req (update req))
  (DELETE "/api/commodities/:id" req (delete req)))
