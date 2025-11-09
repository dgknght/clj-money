(ns clj-money.prices.fetch
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.entities :as entities]
            [clj-money.prices :as p]
            [clj-money.prices.yahoo :as yahoo]
            [clj-money.prices.alpha-vantage :as alpha-vantage]
            [clj-money.prices.cache :as cache]))

(defn- commodity-type?
  [types]
  (fn [{:commodity/keys [type]}]
    (types type)))

(defn- cache-writing-provider
  [provider]
  (reify p/PriceProvider
    (fetch-prices [_ symbols]
      (let [prices (p/fetch-prices provider symbols)]
        (when (seq prices)
          (->> prices
               (map (fn [{:price/keys [value trade-date]
                          :commodity/keys [symbol exchange]}]
                      {:cached-price/value value
                       :cached-price/trade-date trade-date
                       :cached-price/symbol symbol
                       :cached-price/exchange exchange}))
               entities/put-many
               doall))
        prices))))

(def ^:private ->key
  (juxt :commodity/exchange
        :commodity/symbol))

(defn- apply-commodity
  [mapped-commodities]
  (fn [price]
    (-> price
        (assoc :price/commodity (mapped-commodities (->key price)))
        (dissoc :commodity/exchange
                :commodity/symbol))))

(defn- fetch-prices
  [{:keys [types provider]} commodities]
  (->> commodities
       (filter (commodity-type? types))
       (map :commodity/symbol)
       (p/fetch-prices provider)))

(defn- remove-commodities
  "Given a list of commodities, remove the commodities that have a price
  in the given list of prices"
  [prices commodities]
  (let [ids (->> prices
                 (map (comp :id
                            :price/commodity))
                 set)]
    (remove (comp ids :id) commodities)))

(defn- providers []
  [{:provider (cache/->CacheProvider)
    :types #{:fund :stock :currency}}
   {:provider (cache-writing-provider
                (yahoo/->YahooProvider))
    :types #{:fund :stock}}
   {:provider (cache-writing-provider
                (alpha-vantage/->AlphaVantageProvider))
    :types #{:currency :stock :fund}}])

(defn fetch
  "Given a sequence of commodity entities, fetches prices from external services
  and returns the price entities."
  [commodities]
  (let [mapped-commodities (index-by ->key commodities)]
    (loop [providers (providers)
           comms commodities
           prices []]
      (let [provider (first providers)]
        (if (and provider (seq comms))
          (let [fetched (mapv (apply-commodity mapped-commodities)
                              (fetch-prices provider
                                            commodities))]
            (recur (rest providers)
                   (remove-commodities prices comms)
                   (concat prices fetched)))
          prices)))))
