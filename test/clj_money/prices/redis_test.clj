(ns clj-money.prices.redis-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [taoensso.carmine :as car]
            [clj-money.redis-util :refer [reset]]
            [clj-money.config :refer [env]]
            [clj-money.prices :as prices]
            [clj-money.prices.redis :as redis]))

(def ^:private base-config
  {:host (:redis-host env "localhost")
   :port (:redis-port env 6379)
   :password (:redis-password env)})

(defn- config
  [inner]
  (assoc base-config
         ::prices/provider ::redis/cache
         :inner inner))

(use-fixtures :each (reset {:spec base-config
                            :pool (car/connection-pool {})}))

(defprotocol Inspectable
  (inspect [_]))

(defn- inspectable-provider
  []
  (let [calls (atom [])]
    (reify

      prices/Provider
      (fetch [_ symbol opts]
        (swap! calls conj [symbol opts])
        {:symbol "AAPL"
         :date (t/local-date 2020 1 1)
         :close 10M})

      Inspectable
      (inspect [_] @calls))))

(deftest fetch-an-uncached-price
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))]
    (let [result (prices/fetch provider
                               "AAPL"
                               {:date (t/local-date 2020 1 1)})]
      (testing "result"
        (is (= {:symbol "AAPL"
                :date (t/local-date 2020 1 1)
                :close 10M}
               result)
            "The price returned by the inner provider is returned")))
    (testing "delegation"
      (let [[c :as cs] (inspect inspectable)]
        (is (= 1 (count cs))
            "The inner provider is called once")
        (is (= "AAPL" (first c))
            "The symbol is passed to the inner provider")
        (is (= {:date (t/local-date 2020 1 1)}
               (second c))
            "The options are passed to the inner provider")))))

(deftest fetch-a-cached-price
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))]
    (prices/fetch provider
                  "AAPL"
                  {:date (t/local-date 2020 1 1)})
    (let [result (prices/fetch provider
                               "AAPL"
                               {:date (t/local-date 2020 1 1)})]
      (is (= {:symbol "AAPL"
              :date (t/local-date 2020 1 1)
              :close 10M}
             result)
          "The cached result is returned"))
    (testing "delegation"
      (let [cs (inspect inspectable)]
        (is (= 1 (count cs))
            "The inner provider is called once")))))
