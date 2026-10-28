(ns clj-money.prices.mock
  (:require [java-time.api :as t]
            [clj-money.prices :as prices]))

(defprotocol Inspectable
  (inspect [_]))

(defmethod prices/provider ::mock
  [_config]
  (reify
    prices/Provider
    (fetch [_ commodity _opts]
      #:price{:trade-date (t/local-date)
              :value (bigdec (rand 100.0))
              :commodity commodity})))
