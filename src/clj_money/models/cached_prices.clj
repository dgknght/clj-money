(ns clj-money.models.cached-prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]))

(defn- trade-date-unique?
  [cached-price]
  (if (:cached-price/trade-date cached-price)
    (= 0 (-> cached-price
             (select-keys [:cached-price/trade-date
                           :cached-price/symbol
                           :cached-price/exchange])
             models/count))
    true))
(v/reg-spec trade-date-unique? {:message "%s already exists"
                                :path [:cached-price/trade-date]})

(s/def :cached-price/symbol string?)
(s/def :cached-price/trade-date t/local-date?)
(s/def :cached-price/price decimal?)
(s/def :cached-price/exchange models/exchanges)
(s/def ::models/cached-price (s/and (s/keys :req [:cached-price/trade-date
                                                  :cached-price/price
                                                  :cached-price/exchange
                                                  :cached-price/symbol])
                                    trade-date-unique?))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))))

(defn create
  [_cached-price]
  (throw (UnsupportedOperationException. "create is deprecated")))
