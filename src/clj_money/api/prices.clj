(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if
                                          uuid
                                          parse-int]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.prices :as p]
            [clj-money.prices.yahoo :as yahoo]
            [clj-money.prices.alpha-vantage :as alpha-vantage]
            [clj-money.prices.cache :as cache]
            [clj-money.models :as models]
            [clj-money.authorization :refer [+scope
                                             authorize]
             :as authorization]
            [clj-money.authorization.prices]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  {:pre [(:trade-date params)]}

  (->  params
      (update-in [:trade-date] #(apply vector :between> (map dates/unserialize-local-date %)))
      (select-keys [:commodity-id :entity-id :trade-date])
      (rename-keys {:commodity-id :price/commodity
                    :entity-id :commodity/entity
                    :trade-date :price/trade-date})
      (update-in-if [:price/commodity] util/->model-ref)
      (update-in-if [:commodity/entity] util/->model-ref)
      (+scope :price authenticated)))

(defn- index
  [req]
  (-> req
      extract-criteria
      (models/select {:sort [[:price/trade-date :desc]]})
      api/response))

(defn- extract-price
  [{:keys [body]}]
  (-> body
      (select-keys [:price/trade-date
                    :price/price])
      (update-in-if [:price/trade-date] dates/unserialize-local-date)
      (update-in-if [:price/price] bigdec)))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-price
      (assoc :price/commodity {:id (:commodity-id params)})
      (authorize ::authorization/create authenticated)
      models/put
      api/creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (models/find-by {:id (uuid (:id params))
                              :price/trade-date (dates/unserialize-local-date (:trade-date params))})
             action
             authenticated))

(defn- update
  [req]
  (or (some-> (find-and-authorize req ::authorization/update)
              (merge (extract-price req))
              models/put
              api/update-response)
      api/not-found))

(defn- delete
  [req]
  (if-let [price (find-and-authorize req ::authorization/destroy)]
    (do
      (models/delete price)
      (api/response))
    api/not-found))

(defn- fetch*
  "Accepts a list of commodities and returns a map mapping
  exchange/symbol to a map containing the commodity and the price"
  [commodities]
  (let [key-fn (juxt (some-fn :exchange :type)
                     :symbol)
        price-map (->> commodities
                       (map (juxt key-fn
                                  #(hash-map :commodity %)))
                       (into {}))]
    (reduce (fn [m {:keys [provider types]}]
              (let [symbols (->> (vals m)
                                 (remove :price)
                                 (filter #(types (get-in % [:commodity :type])))
                                 (map (comp :symbol :commodity)))]
                (if (seq symbols)
                  (reduce #(update-in %1 [(key-fn %2)] assoc :price %2)
                          m
                          (p/fetch-prices provider symbols))
                  (reduced m))))
            price-map
            [{:provider (cache/->CacheProvider)
              :types #{:fund :stock :currency}}
             {:provider (yahoo/->YahooProvider)
              :types #{:fund :stock}}
             {:provider (alpha-vantage/->AlphaVantageProvider)
              :types #{:currency}}])))

(defn- save-prices
  [prices]
  (doseq [{:keys [commodity price]} (filter :price (vals prices))]
    (-> price
        (assoc :commodity-id (:id commodity))
        models/put)
    (when-not (:exchange commodity)
      (models/put (assoc commodity :exchange (:exchange price)))))
  prices)

(defn- fetch
  "Return prices for a specified list of commodities"
  [{:keys [params]}]
  (->> (:commodity-id params)
       (map (comp #(models/find % :commodity)
                  parse-int)
            (:commodity-id params))
       fetch*
       save-prices
       vals
       (map :price)
       api/response))

(def routes
  [["commodities/:commodity-id/prices" {:post {:handler create}}]
   ["prices"
    ["" {:get {:handler index}}]
    ["/fetch" {:get {:handler fetch}}]
    ["/:trade-date/:id" {:patch {:handler update}
                         :delete {:handler delete}}]]])
