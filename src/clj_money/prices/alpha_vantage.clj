(ns clj-money.prices.alpha-vantage
  (:require [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [lambdaisland.uri :refer [uri
                                      assoc-query*
                                      uri-str]]
            [java-time.api :as t]
            [clj-money.dates :as dates]
            [clj-money.prices :as prices]))

(def service-uri (uri "https://www.alphavantage.co/query"))

(def ^:private market "USD")

(defn- currency?
  [{:commodity/keys [type]}]
  (= :currency type))

(defn- get-quote-uri
  [{sym :commodity/symbol :as commodity} api-key]
  (let [query (if (currency? commodity)
                {:function "DIGITAL_CURRENCY_DAILY"
                 :market market}
                {:function "TIME_SERIES_DAILY"})]
    (-> service-uri
        (assoc-query* (assoc query
                             :symbol sym
                             :apikey api-key))
        uri-str)))

(defn- raw-quote
  [{sym :commodity/symbol :as commodity} api-key]
  (let [url (get-quote-uri commodity api-key)
        res (http/get url {:as :json-string-keys})]
    (if (<= 200 (:status res) 299)
      (:body res)
      (throw (ex-info "Unable to fetch the quote" {:symbol sym})))))

(defn- time-series-key
  [commodity]
  (if (currency? commodity)
    "Time Series (Digital Currency Daily)"
    "Time Series (Daily)"))

(defn- ->price
  [[date m] commodity]
  {:price/trade-date (dates/unserialize-local-date date)
   :price/value (bigdec (get-in m ["4. close"]))
   :price/commodity commodity})

(defn- fetch*
  [commodity {:keys [api-key date]}]
  (let [d (or date (t/local-date))]
    (->> (get (raw-quote commodity api-key)
              (time-series-key commodity))
         (map #(->price % commodity))
         (filter #(= d (:price/trade-date %)))
         first)))

(defmethod prices/provider ::provider
  [config]
  (reify
    prices/Provider
    (fetch [_ commodity opts] (fetch* commodity (merge config opts)))))
