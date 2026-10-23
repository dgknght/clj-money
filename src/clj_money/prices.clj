(ns clj-money.prices)

^:deprecated
(defprotocol PriceProvider
  (fetch-prices [p symbols]))

(defprotocol Provider
  (fetch [_ symbol opts] "Fetch prices for the given symbol."))

(defmulti provider ::provider)
