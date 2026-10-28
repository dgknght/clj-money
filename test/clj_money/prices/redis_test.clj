(ns clj-money.prices.redis-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [taoensso.carmine :as car]
            [clj-money.redis-util :refer [reset]]
            [clj-money.config :refer [env]]
            [clj-money.prices :as prices]
            [clj-money.prices.mock :refer [Inspectable
                                           inspect]]
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

(defn- inspectable-provider
  []
  (let [calls (atom [])]
    (reify

      prices/Provider
      (fetch [_ commodity opts]
        (swap! calls conj [commodity opts])
        #:price{:commodity commodity
                :trade-date (t/local-date 2020 1 1)
                :value 10M})

      Inspectable
      (inspect [_] @calls))))

(deftest fetch-an-uncached-price
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))]
    (let [result (prices/fetch provider
                               #:commodity{:symbol "AAPL"
                                           :exchange :nasdaq}
                               {:date (t/local-date 2020 1 1)})]
      (testing "result"
        (is (= #:price{:commodity #:commodity{:symbol "AAPL"
                                              :exchange :nasdaq}
                       :trade-date (t/local-date 2020 1 1)
                       :value 10M}
               result)
            "The price returned by the inner provider is returned")))
    (testing "delegation"
      (let [[c :as cs] (inspect inspectable)]
        (is (= 1 (count cs))
            "The inner provider is called once")
        (is (= #:commodity{:symbol "AAPL"
                           :exchange :nasdaq}
               (first c))
            "The commodity is passed to the inner provider")
        (is (= {:date (t/local-date 2020 1 1)}
               (second c))
            "The options are passed to the inner provider")))))

(deftest fetch-a-cached-price
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))]
    (prices/fetch provider
                  #:commodity{:symbol "AAPL"
                              :exchange :nasdaq}
                  {:date (t/local-date 2020 1 1)})
    (let [result (prices/fetch provider
                               #:commodity{:symbol "AAPL"
                                           :exchange :nasdaq}
                               {:date (t/local-date 2020 1 1)})]
      (is (= #:price{:commodity #:commodity{:symbol "AAPL"
                                            :exchange :nasdaq}
                     :trade-date (t/local-date 2020 1 1)
                     :value 10M}
             result)
          "The cached result is returned"))
    (testing "delegation"
      (let [cs (inspect inspectable)]
        (is (= 1 (count cs))
            "The inner provider is called once")))))

(deftest bypass-the-cache
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))]
    (prices/fetch provider
                  #:commodity{:symbol "AAPL"
                              :exchange :nasdaq}
                  {:date (t/local-date 2020 1 1)})
    (testing "initial cache"
      (prices/fetch provider
                    #:commodity{:symbol "AAPL"
                                :exchange :nasdaq}
                    {:date (t/local-date 2020 1 1)})
      (is (= 1 (count (inspect inspectable)))
          "Only one call has been made to the inner provider"))
    (testing "bypass"
      (prices/fetch provider
                    #:commodity{:symbol "AAPL"
                                :exchange :nasdaq}
                    {:date (t/local-date 2020 1 1)
                     :bypass-cache? true})
      (testing "delegation"
        (is (= 2 (count (inspect inspectable)))
            "The inner provider is called again")))))

(deftest a-cached-price-belongs-to-the-requesting-commodity
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))]
    (prices/fetch provider
                  {:id 1
                   :commodity/symbol "AAPL"
                   :commodity/exchange :nasdaq}
                  {:date (t/local-date 2020 1 1)})
    (let [result (prices/fetch provider
                               {:id 2
                                :commodity/symbol "AAPL"
                                :commodity/exchange :nasdaq}
                               {:date (t/local-date 2020 1 1)})]
      (is (= #:price{:commodity {:id 2
                                 :commodity/symbol "AAPL"
                                 :commodity/exchange :nasdaq}
                     :trade-date (t/local-date 2020 1 1)
                     :value 10M}
             result)
          "The cached price is returned with the requesting commodity"))
    (testing "delegation"
      (is (= 1 (count (inspect inspectable)))
          "The inner provider is called once"))))

(deftest fetch-a-price-for-a-commodity-without-an-exchange
  (let [inspectable (inspectable-provider)
        provider (prices/provider (config inspectable))
        commodity #:commodity{:symbol "BTC"
                              :type :currency}]
    (prices/fetch provider
                  commodity
                  {:date (t/local-date 2020 1 1)})
    (let [result (prices/fetch provider
                               commodity
                               {:date (t/local-date 2020 1 1)})]
      (is (= #:price{:commodity commodity
                     :trade-date (t/local-date 2020 1 1)
                     :value 10M}
             result)
          "The cached price is returned"))
    (testing "delegation"
      (is (= 1 (count (inspect inspectable)))
          "The inner provider is called once"))))
