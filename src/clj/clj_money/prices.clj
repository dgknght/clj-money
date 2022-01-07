(ns clj-money.prices)

(defprotocol PriceProvider
  (fetch-prices [p symbols]))
