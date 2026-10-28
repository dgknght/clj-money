(ns clj-money.prices.alpha-vantage
  (:require [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [lambdaisland.uri :refer [uri
                                      assoc-query
                                      uri-str]]
            [java-time.api :as t]
            [clj-money.dates :as dates]
            [clj-money.prices :as prices]))

(def service-uri (uri "https://www.alphavantage.co/query"))

(defn- get-quote-uri
  [sym api-key]
  (-> service-uri
      (assoc-query :symbol sym
                   :function "TIME_SERIES_DAILY"
                   :apikey api-key)
      uri-str))

(defn- raw-quote
  [sym api-key]
  (let [url (get-quote-uri sym api-key)
        res (http/get url {:as :json-string-keys})]
    (if (<= 200 (:status res) 299)
      (:body res)
      (throw (ex-info "Unable to fetch the quote" {:symbol sym})))))

(defn- ->price
  [[date m] commodity]
  {:price/trade-date (dates/unserialize-local-date date)
   :price/value (bigdec (get-in m ["4. close"]))
   :price/commodity commodity})

(defn- fetch*
  [{:as commodity sym :commodity/symbol} {:keys [api-key date]}]
  (let [d (or date (t/local-date))]
    (->> (get-in (raw-quote sym api-key) ["Time Series (Daily)"])
         (map #(->price % commodity))
         (filter #(= d (:price/trade-date %)))
         first)))

(defmethod prices/provider ::provider
  [config]
  (reify
    prices/Provider
    (fetch [_ commodity opts] (fetch* commodity (merge config opts)))))
