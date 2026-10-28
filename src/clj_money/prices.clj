(ns clj-money.prices
  (:require [integrant.core :as ig]))

(defprotocol Provider
  (fetch [_ symbol opts] "Fetch prices for the given symbol."))

(defmulti provider ::provider)

(defmethod ig/init-key ::provider
  [_ config]
  (provider config))
