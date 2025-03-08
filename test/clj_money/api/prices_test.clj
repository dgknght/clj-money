(ns clj-money.api.prices-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [java-time.api :as t]
            [clj-money.dates :as dates :refer [with-fixed-time]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [with-context
                                            realize
                                            find-user
                                            find-price
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.prices.yahoo :as yahoo]
            [clj-money.models.prices :as prices]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :commodities [{:name "Apple, Inc"
                  :entity-id "Personal"
                  :symbol "AAPL"
                  :exchange :nasdaq
                  :type :stock}
                 {:name "Some Fund"
                  :entity-id "Personal"
                  :symbol "FND"
                  :exchange :nasdaq
                  :type :fund}]})

(defn- create-a-price
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        commodity (find-commodity ctx "AAPL")
        response (-> (req/request :post (path :api
                                              :commodities
                                              (:id commodity)
                                              :prices))
                     (edn-body {:price 12.34M
                                :trade-date (t/local-date 2016 3 2)})
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (prices/search {:commodity-id (:id commodity)
                                  :trade-date (t/local-date 2016 3 2)})]
    [response retrieved]))

(defn- assert-successful-create
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected {:price 12.34M
                  :trade-date (t/local-date 2016 3 2)}]
    (is (comparable? expected edn-body)
        "The created price is returned in the response")
    (is (comparable? expected (first retrieved))
        "The price is created in the database")))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved) "The database record is not created"))

(deftest a-user-can-create-a-price-for-a-commodity-in-his-entity
  (assert-successful-create (create-a-price "john@doe.com")))

(deftest a-user-cannot-create-a-price-for-a-commodity-in-anothers-entity
  (assert-blocked-create (create-a-price "jane@doe.com")))

(def ^:private list-context
  (assoc context :prices [{:commodity-id "AAPL"
                           :trade-date (t/local-date 2016 2 27)
                           :price 10M}
                          {:commodity-id "AAPL"
                           :trade-date (t/local-date 2016 3 2)
                           :price 11M}
                          {:commodity-id "FND"
                           :trade-date (t/local-date 2016 2 27)
                           :price 10.01M}
                          {:commodity-id "FND"
                           :trade-date (t/local-date 2016 3 2)
                           :price 9.99M}]))

(defn- get-a-list-by-commodity
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        commodity (find-commodity ctx "AAPL")]
    (-> (req/request :get (str (path :api :prices)
                               "?"
                               (map->query-string {:trade-date [(t/local-date 2016 1 1) (t/local-date 2016 12 31)]
                                                   :commodity-id (:id commodity)})))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= [{:trade-date (t/local-date 2016 3 2)
           :price 11.0M}
          {:trade-date (t/local-date 2016 2 27)
           :price 10.0M}]
         (map #(select-keys % [:trade-date
                               :price])
              edn-body))))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-prices-from-his-entity
  (assert-successful-list (get-a-list-by-commodity "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-prices-from-anothers-entity
  (assert-blocked-list (get-a-list-by-commodity "jane@doe.com")))

(defn- update-a-price
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        price (find-price ctx "AAPL" (t/local-date 2016 2 27))
        response (-> (req/request :patch (path :api
                                               :prices
                                               (dates/serialize-local-date (:trade-date price))
                                               (:id price)))
                     (edn-body {:trade-date (t/local-date 2016 2 27)
                                :price 9.99M})
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (prices/find price)]
    [response retrieved]))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected {:trade-date (t/local-date 2016 2 27)
                  :price 9.99M}]
    (is (comparable? expected edn-body)
        "The response contains the updated price")
    (is (comparable? expected retrieved)
        "The database record is updated")))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:trade-date (t/local-date 2016 2 27)
                    :price 10M}
                   retrieved)
      "The database record is not updated"))

(deftest a-user-can-update-a-price-for-a-commodity-is-his-entity
  (assert-successful-update (update-a-price "john@doe.com")))

(deftest a-user-cannot-update-a-price-for-a-commodity-in-anothers-entity
  (assert-blocked-update (update-a-price "jane@doe.com")))

(defn- delete-a-price
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        price (find-price ctx "AAPL" (t/local-date 2016 2 27))
        response (-> (req/request :delete (path :api
                                                :prices
                                                (dates/serialize-local-date (:trade-date price))
                                                (:id price)))
                     (add-auth user)
                     app)
        retrieved (prices/find price)]
    [response retrieved]))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The price cannot be retrieved after delete."))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The price can still be retrieved after blocked delete."))

(deftest a-user-can-delete-a-price-in-his-entity
  (assert-successful-delete (delete-a-price "john@doe.com")))

(deftest a-user-cannot-delete-a-price-in-anothers-entity
  (assert-blocked-delete (delete-a-price "jane@doe.com")))

(def ^:private fetch-context
  {:users [{:first-name "John"
            :last-name "Doe"
            :email "john@doe.com"
            :password "please01"}]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]
   :commodities [{:entity-id "Personal"
                  :name "Apple, Inc."
                  :symbol "AAPL"
                  :exchange :nasdaq
                  :type :stock}
                 {:entity-id "Personal"
                  :name "Microsoft, Inc."
                  :symbol "MSFT"
                  :exchange :nasdaq
                  :type :stock}]})

(defn- fetch-some-prices
  [username]
  (let [user (find-user username)
        appl (find-commodity "AAPL")
        msft (find-commodity "MSFT")]
    (with-redefs [yahoo/get-quotes (fn [symbols]
                                     (map (fn [s]
                                            {:symbol s
                                             :fullExchangeName "NasdaqGS"
                                             :regularMarketPrice 10.01M
                                             :regularMarketTime (t/local-date 2015 3 2)})
                                          symbols))]
      (with-fixed-time "2015-03-02T12:00:00Z"
        (-> (req/request :get (str (path :api
                                         :prices
                                         :fetch)
                                   "?commodity-id="
                                   (:id appl)
                                   "&commodity-id="
                                   (:id msft)))
            (add-auth user)
            app
            parse-edn-body)))))

(defn- assert-successful-fetch
  [response]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:trade-date (t/local-date 2015 3 2)
                           :price 10.01M}
                          {:trade-date (t/local-date 2015 3 2)
                           :price 10.01M}]
                         (:edn-body response))
      "The response contains the matching prices"))

(deftest a-user-can-fetch-a-current-commodity-prices
  (with-context fetch-context
    (assert-successful-fetch (fetch-some-prices "john@doe.com"))))

(deftest an-unauthenticated-user-cannot-fetch-commodity-prices
  (with-context fetch-context
    (is (http-unauthorized? (fetch-some-prices nil)))))

(deftest prices-are-only-fetched-once-per-day
  (with-context fetch-context
    (let [user (find-user "john@doe.com")
          appl (find-commodity "AAPL")
          msft (find-commodity "MSFT")
          calls (atom [])]
      (with-redefs [yahoo/get-quotes (fn [symbols]
                                       (swap! calls conj symbols)
                                       (map (fn [s]
                                              {:symbol s
                                               :regularMarketPrice 10.01M
                                               :regularMarketTime (t/local-date 2015 3 2)
                                               :fullExchangeName "NasdaqGS"})
                                            symbols))]
        (t/with-clock (t/fixed-clock (t/instant (t/formatter :iso-instant) "2015-03-02T12:00:00Z"))
          (-> (req/request :get (str (path :api
                                           :prices
                                           :fetch)
                                     "?commodity-id="
                                     (:id appl)
                                     "&commodity-id="
                                     (:id msft)))
              (add-auth user)
              app
              parse-edn-body)
          (-> (req/request :get (str (path :api
                                           :prices
                                           :fetch)
                                     "?commodity-id="
                                     (:id appl)
                                     "&commodity-id="
                                     (:id msft)))
              (add-auth user)
              app
              parse-edn-body))
        (is (= 1 (count @calls))
            "The external service is only called once")))))
