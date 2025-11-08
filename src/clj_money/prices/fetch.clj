(ns clj-money.prices.fetch
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :refer [entity=]]
            [clj-money.entities :as entities]
            [clj-money.prices :as p]
            [clj-money.prices.fetch :as f]
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

(defn- fetch-prices
  [{:keys [types provider mapped-commodities]} commodities]
  (->> commodities
       (filter (commodity-type? types))
       (map :commodity/symbol)
       (p/fetch-prices provider)
       (mapv (fn [p]
               (let [c (mapped-commodities (->key p))]
                 (-> p
                     (assoc :price/commodity c)
                     (dissoc :commodity/exchange
                             :commodity/symbol)))))))

(defn- remove-commodities
  "Given a list of commodities, remove the commodities that have a price
  in the given list of prices"
  [prices commodities]
  (remove (fn [c]
            (some #(entity= (:price/commodity %)
                            c)
                  prices))
          commodities))

(defn- process-commodities
  [mapped-commodities]
  (fn [{:keys [commodities] :as m}
       {:keys [provider types]}]
    (if (empty? commodities)
      (reduced m)
      (let [prices (fetch-prices {:mapped-commodities mapped-commodities
                                  :types types
                                  :provider provider}
                                 commodities)]
        (-> m
            (update-in [:prices] concat prices)
            (update-in [:commodities] (partial remove-commodities prices)))))))

(defn fetch
  "Given a sequence of commodity entities, fetches prices from external services
  and returns the price entities."
  [commodities]
  (let [mapped-commodities (index-by ->key commodities)]
    (:prices (reduce (process-commodities mapped-commodities)
                     {:commodities commodities
                      :prices []}
                     [{:provider (cache/->CacheProvider)
                       :types #{:fund :stock :currency}}
                      {:provider (cache-writing-provider
                                   (yahoo/->YahooProvider))
                       :types #{:fund :stock}}
                      {:provider (cache-writing-provider
                                   (alpha-vantage/->AlphaVantageProvider))
                       :types #{:currency :stock :fund}}]))))
