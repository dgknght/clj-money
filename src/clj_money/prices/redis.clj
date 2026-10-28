(ns clj-money.prices.redis
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.carmine :as car]
            [clj-money.dates :as dates]
            [clj-money.prices :as prices]))

(defn- cache-key
  [{sym :commodity/symbol :commodity/keys [exchange]} {:keys [date] :as opts}]
  {:pre [(:date opts)]}
  (format "clj-money:prices:%s:%s:%s"
          sym
          (if exchange (name exchange) "no-exchange")
          (dates/serialize-local-date date)))

(defn- cached-price
  [k config]
  (car/wcar config (car/get k)))

(defn- put-price
  [price k config]
  (car/wcar config (car/set k price)))

(defn- fetch*
  "Returns the cached price, or fetches it from the provider and caches it.
  With :bypass-cache? in opts, the cached price is ignored and replaced."
  [commodity provider {:keys [bypass-cache?] :as opts} config]
  {:pre [(map? commodity)]}
  (let [k (cache-key commodity opts)]
    (if-let [cached (when-not bypass-cache?
                      (cached-price k config))]
      (assoc cached
             :price/commodity commodity)
      (let [fetched (prices/fetch provider commodity opts)]
        (-> fetched
            (dissoc :price/commodity)
            (put-price k config))
        fetched))))

(defn- provider
  [{:keys [inner] :as config}]
  (let [p (if (satisfies? prices/Provider inner)
            inner
            (prices/provider inner))
        pool (car/connection-pool {})]
    (reify
      prices/Provider
      (fetch [_ commodity opts]
        (fetch* commodity
                p
                opts
                {:pool pool
                 :spec (select-keys config [:uri :host :port :password])})))))

(defmethod prices/provider ::cache
  [config]
  (provider config))
