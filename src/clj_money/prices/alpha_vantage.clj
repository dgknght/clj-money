(ns clj-money.prices.alpha-vantage
  (:require [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [lambdaisland.uri :refer [uri
                                      map->query-string]]
            [clj-money.dates :as dates]
            [clj-money.prices :as prices]))

(def service-uri (uri "https://www.alphavantage.co/query"))

(defn- get-quote-uri
  [sym api-key]
  (-> service-uri
      (assoc :query (map->query-string {:symbol sym
                                        :function "TIME_SERIES_DAILY"
                                        :apikey api-key}))
      str))

(defn- raw-quote
  [sym api-key]
  (let [url (get-quote-uri sym api-key)
        res (http/get url {:as :json-string-keys})]
    (if (<= 200 (:status res) 299)
      (:body res)
      (throw (ex-info "Unable to fetch the quote" {:symbol sym})))))

(defn- xform-quote
  [[date m] sym]
  {:price/trade-date (dates/unserialize-local-date date)
   :price/value (bigdec (get-in m ["4. close"]))
   :commodity/symbol sym})

(defn- fetch*
  [sym {:keys [api-key date]}]
  (->> (get-in (raw-quote sym api-key) ["Time Series (Daily)"])
       (map #(xform-quote % sym))
       (filter #(= date (:date %)))
       first))

(defmethod prices/provider ::provider
  [config]
  (reify
    prices/Provider
    (fetch [_ symbol opts] (fetch* symbol (merge config opts)))))
