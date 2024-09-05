(ns clj-money.prices.yahoo
  (:require [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clj-http.client :as http]
            [java-time.api :as t]
            [config.core :refer [env]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [lambdaisland.uri :refer [uri
                                      map->query-string]]
            [clj-money.prices :as prices]
            [clj-money.models.cached-prices :as cached-prices]))

(def service-uri (uri "https://yh-finance.p.rapidapi.com/market/v2/get-quotes"))
(def rapidapi-host "yh-finance.p.rapidapi.com")

(defn- get-quotes-uri
  [symbols]
  (-> service-uri
      (assoc :query (map->query-string {:symbols (string/join "," symbols)
                                        :region "US"}))
      str))

(defn get-quotes
  [symbols]
  (let [url (get-quotes-uri symbols)
        _ (log/infof "get-quotes %s" url)
        res (http/get url {:as :json
                           :headers {:x-rapidapi-host rapidapi-host
                                     :x-rapidapi-key (env :yahoo-api-key)}})]
    (log/debugf "get-quotes %s: %s" url (prn-str res))
    (->> (get-in res [:body :quoteResponse :result])
         (map (fn [m]
                (-> m ; There are like 1,000 other conversions we could do, but this is all we need right now
                    (update-in [:regularMarketTime] (comp t/instant
                                                          #(.longValue %)
                                                          (partial * 1000)))
                    (update-in [:regularMarketPrice] bigdec)))))))

(defn cache
  [price]
  (cached-prices/create price)
  price)

(def ^:private exchange-names
  {"NasdaqGS" :nasdaq
   "Other OTC" :otc})

(defn- map-exchange-name
  [yahoo-name]
  (when yahoo-name
    (get-in exchange-names [yahoo-name] (->kebab-case-keyword yahoo-name))))

(deftype YahooProvider []
  prices/PriceProvider
  (prices/fetch-prices [_ symbols]
    (->> symbols
         get-quotes
         (map #(-> %
                   (rename-keys {:regularMarketPrice :price
                                 :regularMarketTime :trade-date
                                 :fullExchangeName :exchange})
                   (update-in [:exchange] map-exchange-name)
                   (select-keys [:price
                                 :trade-date
                                 :symbol
                                 :exchange])
                   cache)))))
