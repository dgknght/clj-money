(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.authorization.commodities]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (rename-keys {:entity-id :commodity/entity})
      (update-in [:commodity/entity] #(hash-map :id %))
      (+scope :commodity authenticated)))

(defn- count
  [req]
  (api/response
    {:count (models/count (extract-criteria req))}))

(defn- append-current-prices
  [commodities]
  ; TODO: performance tuning opportunity - reduces the number of queries here
  (map (fn [{:as comm :commodity/keys [earliest-price latest-price]}]
         (assoc comm
                :commodity/most-recent-price
                (models/find-by {:price/commodity comm
                                 :price/trade-date [:between
                                                    earliest-price
                                                    latest-price]}
                                {:sort [[:price/trade-date :desc]]})))
       commodities))

(defn- append-shares-owned
  [commodities]
  (if (seq commodities)
    (let [lots (->> (models/select #:lot{:commodity {:id [:in (map :id commodities)]}
                                         :shares-owned [:> 0]})
                    (group-by (comp :lot/commodity :id))
                    (map #(update-in % [1] (fn [lots]
                                             (->> lots
                                                  (map :lot/shares-owned)
                                                  (reduce + 0M)))))
                    (into {}))]
      (map #(assoc % :lot/shares-owned (get-in lots [(:id %)] 0M))
           commodities))
    commodities))

(defn- index
  [req]
  (->> (models/select (extract-criteria req))
       append-current-prices
       append-shares-owned
       api/response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (-> params
      (select-keys [:id])
      (+scope :commodity authenticated)
      models/find-by
      (authorize action authenticated)))

(defn- show
  [req]
  (if-let [comm (find-and-authorize req ::authorization/show)]
    (api/response comm)
    api/not-found))

(def ^:private attribute-keys
  [:id
   :commodity/entity
   :commodity/name
   :commodity/symbol
   :commodity/exchange
   :commodity/type
   :commodity/price-config])

(defn- create
  [{:keys [authenticated params]}]
  (-> params
      (select-keys attribute-keys)
      (authorize ::authorization/create authenticated)
      models/put
      api/creation-response))

(defn- update
  [{:keys [params] :as req}]
  (if-let [commodity (find-and-authorize req ::authorization/update)]
    (-> commodity
        (merge (select-keys params attribute-keys))
        models/put
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [commodity (find-and-authorize req ::authorization/destroy)]
    (do
      (models/delete commodity)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id/commodities"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/count" {:get {:handler count}}]]
   ["commodities/:id" {:get {:handler show}
                       :patch {:handler update}
                       :delete {:handler delete}}]])
