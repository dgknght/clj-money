(ns clj-money.prices.alpha-vantage
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clj-http.client :as http]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [environ.core :refer [env]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [lambdaisland.uri :refer [uri
                                      map->query-string]]
            [clj-money.prices :as prices]
            [clj-money.models.cached-prices :as cached-prices]))

(def service-uri (uri "https://alpha-vantage.p.rapidapi.com/query"))
(def rapidapi-host "alpha-vantage.p.rapidapi.com")

(def date-time-formatter
  (tf/formatter "yyyy-MM-dd HH:mm:ss"))

(def date-formatter
  (tf/formatters :date))

(defn- get-quote-uri
  [sym]
  (-> service-uri
      (assoc :query (map->query-string {:symbol sym
                                        :function "DIGITAL_CURRENCY_DAILY"
                                        :market "USD"}))
      str))

(defn strip-ordinal [s]
  (string/replace s #"^\d+[ab]?\. " ""))

(defn strip-parenthetical [s]
  (string/replace s #"\s?\([^)]+\)" ""))

(defn- adj-key? [k]
  (and (string? k)
       (not (re-find #"[A-Z]{3}" k))))

(def ^:private adj-key 
  (comp ->kebab-case-keyword
        strip-parenthetical
        strip-ordinal))

(defn- adj-keys [m]
  (transform-keys #(if (adj-key? %)
                     (adj-key %)
                     %)
                  m))

(defn- extract-currency-map [m]
  (reduce-kv (fn [r k v]
               (let [[_ quote-type currency] (re-find #"([a-z ]+)(?: \(([A-Z]+)\))?$" k)]
                 (if currency
                   (update-in r [currency] assoc quote-type (bigdec v))
                   (assoc r quote-type (bigdec v)))))
             {}
             m))

(defn- extract-daily-currency-maps [m]
  (update-in m
             ["Time Series (Digital Currency Daily)"]
             (fn [series-map]
               (->> series-map
                    (map #(update-in % [1] extract-currency-map))
                    (map #(update-in % [0] (partial tf/parse-local-date date-formatter)))
                    (into {})))))

(defn get-quote
  [sym]
  (let [url (get-quote-uri sym)
        _ (log/infof "get-quote %s" url)
        res (http/get url {:as :json-string-keys
                           :headers {:x-rapidapi-host rapidapi-host
                                     :x-rapidapi-key (env :alpha-vantage-api-key)}})]
    (log/debugf "get-response response %s: %s" url (prn-str res))
    (-> (get-in res [:body])
        extract-daily-currency-maps
        adj-keys
        (update-in [:meta-data :last-refreshed] (partial tf/parse date-time-formatter)))))

(defn cache
  [price]
  (cached-prices/create price)
  price)

(defn- transform-quote
  [m]
  (let [trade-date (tc/to-local-date (get-in m [:meta-data :last-refreshed]))]
    {:price (get-in m [:time-series trade-date "USD" :close])
     :symbol (get-in m [:meta-data :digital-currency-code])
     :exchange :currency
     :trade-date trade-date}))

(deftype AlphaVantageProvider []
  prices/PriceProvider
  (prices/fetch-prices [_ symbols]
    (mapv (comp transform-quote
                get-quote)
          symbols)))
