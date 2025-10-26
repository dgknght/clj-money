(ns clj-money.entities.cached-prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities :as entities]))

(defn- trade-date-unique?
  [cached-price]
  (if (:cached-price/trade-date cached-price)
    (= 0 (-> cached-price
             (select-keys [:cached-price/trade-date
                           :cached-price/symbol
                           :cached-price/exchange])
             entities/count))
    true))
(v/reg-spec trade-date-unique? {:message "%s already exists"
                                :path [:cached-price/trade-date]})

(s/def :cached-price/symbol string?)
(s/def :cached-price/trade-date t/local-date?)
(s/def :cached-price/value decimal?)
(s/def :cached-price/exchange entities/exchanges)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/cached-price (s/and (s/keys :req [:cached-price/trade-date
                                                  :cached-price/value
                                                  :cached-price/exchange
                                                  :cached-price/symbol])
                                    trade-date-unique?))
