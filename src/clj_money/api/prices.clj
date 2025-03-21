(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.core :as storage]
            [dgknght.app-lib.core :refer [uuid
                                          update-in-if
                                          parse-int]]
            [dgknght.app-lib.authorization :refer [+scope
                                             authorize]
             :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.prices :as p]
            [clj-money.prices.yahoo :as yahoo]
            [clj-money.prices.alpha-vantage :as alpha-vantage]
            [clj-money.prices.cache :as cache]
            [clj-money.models :as models]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.authorization.prices]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  {:pre [(:trade-date params)]}

  (->  params
      (update-in [:trade-date] #(apply vector :between> (map dates/unserialize-local-date %)))
      (select-keys [:commodity-id :entity-id :trade-date])
      (+scope ::models/price authenticated)))

(defn- index
  [req]
  (api/response
    (prices/search (extract-criteria req)
                   {:sort [[:trade-date :desc]]})))

(defn- ensure-local-date
  [d]
  (if (string? d)
    (dates/unserialize-local-date d)
    d))

(defn- extract-price
  [{:keys [params]}]
  (-> params
      (select-keys [:commodity-id :trade-date :price])
      (update-in-if [:trade-date] ensure-local-date)
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
                          (:trade-date params))
             action
             authenticated))

(defn- update
  [req]
  (or (some-> (scoped-find req ::authorization/update)
              (merge (extract-price req))
              prices/update
              api/update-response)
      api/not-found))

(defn- delete
  [req]
  (if-let [price (scoped-find req ::authorization/destroy)]
    (do
      (prices/delete price)
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
        prices/create)
    (when-not (:exchange commodity)
      (commodities/update (assoc commodity :exchange (:exchange price)))))
  prices)

(defn- fetch
  "Return prices for a specified list of commodities"
  [{:keys [params]}]
  (->> (:commodity-id params)
       (map (comp commodities/find
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
