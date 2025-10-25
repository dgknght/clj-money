(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.entities :as models]
            [clj-money.entities.propagation :as prop]
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

(defn- fetch-current-price
  [{:as commodity :commodity/keys [price-date-range]}]
  (when price-date-range
    (models/find-by #:price{:commodity commodity
                            :trade-date (apply vector :between price-date-range)}
                    {:sort [[:price/trade-date :desc]]})))

(defn- append-current-prices
  [commodities]
  ; TODO: performance tuning opportunity - reduces the number of queries here
  (map #(assoc %
               :commodity/most-recent-price
               (fetch-current-price %))
       commodities))

(defn- append-shares-owned
  [commodities]
  (if (seq commodities)
    (let [lots (->> (models/select #:lot{:commodity [:in (map util/->model-ref commodities)]
                                         :shares-owned [:> 0]})
                    (group-by (comp :id :lot/commodity))
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
  (->> (models/select (extract-criteria req)
                      {:sort [:commodity/symbol]})
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
      (assoc :commodity/entity {:id (:entity-id params)})
      (authorize ::authorization/create authenticated)
      prop/put-and-propagate
      api/creation-response))

(defn- update
  [{:keys [params] :as req}]
  (or
    (some-> (find-and-authorize req ::authorization/update)
            (merge (select-keys params attribute-keys))
            prop/put-and-propagate
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
