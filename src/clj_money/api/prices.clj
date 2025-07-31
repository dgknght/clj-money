(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if
                                          uuid
                                          index-by]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util :refer [model=]]
            [clj-money.comparatives :as comparatives]
            [clj-money.dates :as dates]
            [clj-money.prices :as p]
            [clj-money.prices.yahoo :as yahoo]
            [clj-money.prices.alpha-vantage :as alpha-vantage]
            [clj-money.prices.cache :as cache]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]
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
      (update-in-if [:price/commodity] util/->model-ref)
      (update-in-if [:commodity/entity] util/->model-ref)
      (+scope :price authenticated)
      (util/model-type :price)))

(defn- index
  [req]
  (-> req
      extract-criteria
      (models/select {:sort [[:price/trade-date :desc]]})
      api/response))

(defn- create
  [{:keys [authenticated params]}]
  (-> params
      (select-keys [:price/trade-date
                    :price/value])
      (assoc :price/commodity {:id (:commodity-id params)})
      (authorize ::authorization/create authenticated)
      prop/put-and-propagate
      api/creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (models/find-by {:id (uuid (:id params))
                              :price/trade-date (dates/unserialize-local-date (:trade-date params))})
             action
             authenticated))

(defn- update
  [{:as req :keys [params]}]
  (or (some-> (find-and-authorize req ::authorization/update)
              (merge (select-keys params [:price/value
                                          :price/trade-date]))
              prop/put-and-propagate
              api/update-response)
      api/not-found))

(defn- delete
  [req]
  (if-let [price (find-and-authorize req ::authorization/destroy)]
    (do
      (models/delete price)
      (api/response))
    api/not-found))

(defn- commodity-type?
  [types]
  (fn [{:commodity/keys [type]}]
    (types type)))

; TODO: Move this to a better place
(defn- fetch*
  "Given a sequence of commodity models, fetches prices from external services
  and returns the price models."
  [commodities]
  (let [->key (juxt :commodity/exchange
                    :commodity/symbol)
        mapped-commodities (index-by ->key commodities)
        with-cache-writes (fn [p]
                            (reify p/PriceProvider
                              (fetch-prices [_ symbols]
                                (let [prices (p/fetch-prices p symbols)]
                                  (->> prices
                                       (map (fn [{:price/keys [value trade-date]
                                                  :commodity/keys [symbol exchange]}]
                                              {:cached-price/value value
                                               :cached-price/trade-date trade-date
                                               :cached-price/symbol symbol
                                               :cached-price/exchange exchange}))
                                       models/put-many
                                       doall)
                                  prices))))]
    (:prices (reduce (fn [{:keys [commodities] :as m}
                          {:keys [provider types]}]
                       (if (empty? commodities)
                         (reduced m)
                         (let [prices (->> commodities
                                           (filter (commodity-type? types))
                                           (map :commodity/symbol)
                                           (p/fetch-prices provider)
                                           (mapv (fn [p]
                                                   (let [c (mapped-commodities (->key p))]
                                                     (-> p
                                                         (assoc :price/commodity c)
                                                         (dissoc :commodity/exchange
                                                                 :commodity/symbol))))))]
                           (-> m
                               (update-in [:prices] concat prices)
                               (update-in [:commodities] (fn [cs]
                                                           (remove (fn [c]
                                                                     (some #(model= (:price/commodity %)
                                                                                    c)
                                                                           prices))
                                                                   cs)))))))
                     {:commodities commodities
                      :prices []}
                     [{:provider (cache/->CacheProvider)
                       :types #{:fund :stock :currency}}
                      {:provider (with-cache-writes
                                   (yahoo/->YahooProvider))
                       :types #{:fund :stock}}
                      {:provider (with-cache-writes
                                   (alpha-vantage/->AlphaVantageProvider))
                       :types #{:currency}}]))))

(defn- fetch
  "Return prices for a specified list of commodities"
  [{:keys [params]}]
  (->> (:commodity-id params)
       (map (models/find :commodity))
       fetch*
       models/put-many
       (map #(update-in % [:price/commodity] util/->model-ref))
       api/response))

(def routes
  [["commodities/:commodity-id/prices" {:post {:handler create}
                                        :get {:handler index}}]
   ["prices"
    ["" {:get {:handler index}}]
    ["/fetch" {:get {:handler fetch}}]
    ["/:trade-date/:id" {:patch {:handler update}
                         :delete {:handler delete}}]]])
