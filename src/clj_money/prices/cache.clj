(ns clj-money.prices.cache
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.prices :as prices]
            [clj-money.entities :as entities]))

(deftype CacheProvider []
  prices/PriceProvider
  (fetch-prices [_ symbols]
    (map (fn [{:cached-price/keys [price trade-date exchange symbol]}]
           {:price/value price
            :price/trade-date trade-date
            :commodity/exchange exchange
            :commodity/symbol symbol})
         (entities/select #:cached-price{:trade-date (t/local-date)
                                       :symbol [:in symbols]}))))
