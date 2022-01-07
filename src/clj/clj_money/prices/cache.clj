(ns clj-money.prices.cache
  (:require [clj-time.core :as t]
            [clj-money.prices :as prices]
            [clj-money.models.cached-prices :as cached-prices]))

(deftype CacheProvider []
  prices/PriceProvider
  (fetch-prices [_ symbols]
    (cached-prices/search {:trade-date (t/today)
                           :symbol symbols})))
