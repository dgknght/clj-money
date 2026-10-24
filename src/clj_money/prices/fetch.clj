(ns clj-money.prices.fetch
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.config :refer [env]]
            [clj-money.prices :as prices]
            [clj-money.prices.ref]))

(defn fetch
  "Given a sequence of commodity entities, fetches prices from external services
  and returns the price entities. The config holds the price API keys from
  clj-money.services: :yahoo-api-key and :alpha-vantage-api-key."
  [commodities]
  (let [p (prices/provider (:prices env))]
    (map #(prices/fetch p (:commodity/symbol %) {})
         commodities)))
