(ns clj-money.prices.redis
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.carmine :as car]
            [clj-money.dates :as dates]
            [clj-money.prices :as prices]))

(defn- cache-key
  [symbol {:keys [date]}]
  (format "clj-money:prices:%s:%s"
          symbol
          (dates/serialize-local-date date)))

(defn- cached-price
  [k config]
  (car/wcar config (car/get k)))

(defn- put-price
  [price k config]
  (car/wcar config (car/set k price)))

(defn- fetch*
  [symbol provider opts config]
  (let [k (cache-key symbol opts)]
    (if-let [cached (cached-price k config)]
      cached
      (let [fetched (prices/fetch provider symbol opts)]
        (put-price fetched k config)
        fetched))))

(defn- provider
  [{:keys [inner] :as config}]
  (let [p (if (satisfies? prices/Provider inner)
            inner
            (prices/provider inner))
        pool (car/connection-pool {})]
    (reify
      prices/Provider
      (fetch [_ symbol opts]
        (fetch* symbol
                p
                opts
                {:pool pool
                 :spec (select-keys config [:uri :host :port :password])})))))

(defmethod prices/provider ::cache
  [config]
  (provider config))
