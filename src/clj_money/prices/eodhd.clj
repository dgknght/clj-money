(ns clj-money.prices.eodhd
  (:require [clojure.string :as str]
            [clj-http.client :as http]
            [java-time.api :as t]
            [lambdaisland.uri :refer [uri
                                      uri-str
                                      assoc-query]]
            [clj-money.dates :as dates]
            [clj-money.prices :as prices]))

(def ^:private base-uri "https://eodhd.com")

(def ^:private exchange-codes
  {:nyse "US"
   :nasdaq "US"
   :amex "US"
   :otc "US"})

(defn- api-uri
  [{sym :commodity/symbol :commodity/keys [exchange]}
   {:keys [api-key date]}]
  (-> (uri base-uri)
      (assoc :path (str/join "/" ["/api"
                                  "eod"
                                  (format "%s.%s"
                                          sym
                                          (exchange-codes exchange "US"))]))
      (assoc-query :api_token api-key
                   :fmt "json"
                   :from (dates/serialize-local-date (t/minus date (t/days 1)))
                   :to (dates/serialize-local-date (t/plus date (t/days 1))))
      uri-str))

(defn- request
  [commodity opts]
  (let [res (http/get (api-uri commodity opts)
                      {:as :json})]
    (if (<= 200 (:status res) 299)
      (:body res)
      (throw (ex-info "Unable to fetch the price." {:commodity commodity})))))

(defn- ->price
  [commodity]
  (fn [{:keys [date close]}]
    #:price{:trade-date (dates/unserialize-local-date date)
            :value (bigdec close)
            :commodity commodity}))

(defn- fetch*
  [commodity {:as opts :keys [date]}]
  {:pre [(:date opts)]}
  (->> (request commodity opts)
       (map (->price commodity))
       (filter #(= date (:price/trade-date %)))
       first))

(defmethod prices/provider ::provider
  [config]
  (reify
    prices/Provider
    (fetch [_ commodity opts] (fetch* commodity (merge config opts)))))
