(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.comparatives :as comparatives]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.prices.fetch :as fetch]
            [clj-money.authorization :refer [+scope
                                             authorize]
             :as authorization]
            [clj-money.authorization.prices]))

(defn- unserialize-date
  [x]
  (cond
    (vector? x) (mapv unserialize-date x)
    (string? x) (dates/unserialize-local-date x)
    :else x))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (->  params
      comparatives/symbolize
      (update-in [:trade-date] unserialize-date)
      (select-keys [:commodity-id :entity-id :trade-date])
      (rename-keys {:commodity-id :price/commodity
                    :entity-id :commodity/entity
                    :trade-date :price/trade-date})
      (update-in-if [:price/commodity] util/->entity-ref)
      (update-in-if [:commodity/entity] util/->entity-ref)
      (+scope :price authenticated)
      (util/entity-type :price)))

(defn- index
  [req]
  (-> req
      extract-criteria
      (entities/select {:sort [[:price/trade-date :desc]]})
      api/response))

(defn- extract-price
  [{:keys [params]}]
  (-> params
      (select-keys [:price/trade-date
                    :price/value])
      (update-in-if [:price/value] bigdec)
      (update-in-if [:price/trade-date] dates/ensure-local-date)))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> (extract-price req)
      (assoc :price/commodity {:id (:commodity-id params)})
      (authorize ::authorization/create authenticated)
      prop/put-and-propagate
      api/creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (entities/find-by {:id (:id params)
                              :price/trade-date (dates/unserialize-local-date (:trade-date params))})
             action
             authenticated))

(defn- update
  [req]
  (or (some-> (find-and-authorize req ::authorization/update)
              (merge (extract-price req))
              prop/put-and-propagate
              api/update-response)
      api/not-found))

(defn- delete
  [req]
  (if-let [price (find-and-authorize req ::authorization/destroy)]
    (do
      (entities/delete price)
      (api/response))
    api/not-found))

(defn- fetch
  "Return prices for a specified list of commodities"
  [{:keys [params]}]
  (->> (:commodity-id params)
       (map (entities/find :commodity))
       fetch/fetch
       entities/put-many
       (map #(update-in % [:price/commodity] util/->entity-ref))
       api/response))

(def routes
  [["commodities/:commodity-id/prices" {:post {:handler create}
                                        :get {:handler index}}]
   ["prices"
    ["" {:get {:handler index}}]
    ["/fetch" {:get {:handler fetch}}]
    ["/:trade-date/:id" {:patch {:handler update}
                         :delete {:handler delete}}]]])
