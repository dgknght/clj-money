(ns clj-money.api.prices-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-time.core :as t]
            [clj-money.x-platform.util :refer [map->query-string
                                               serialize-date
                                               path]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.serialization :as serialization]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-not-found]]
            [clj-money.test-helpers :refer [reset-db
                                            selective=
                                            find-user
                                            find-price
                                            find-commodity]]
            [clj-money.models.prices :as prices]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each (partial reset-db (env :db)))

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
                  :type :fund}]})

(defn- create-a-price
  [email]
  (let [ctx (serialization/realize (env :db) context)
        user (find-user ctx email)
        commodity (find-commodity ctx "AAPL")
        response (-> (req/request :post (path :api
                                              :commodities
                                              (:id commodity)
                                              :prices))
                     (req/json-body {:price 12.34
                                     :trade-date "2016-03-02"})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (prices/search (env :db) {:commodity-id (:id commodity)
                                            :trade-date (t/local-date 2016 3 2)})]
    [response body retrieved]))

(defn- assert-successful-create
  [[response body retrieved]]
  (assert-successful response)
  (is (selective= {:price 12.34
                   :trade-date "2016-03-02"}
                  body)
      "The created price is returned in the response")
  (is (selective= {:price 12.34M
                   :trade-date (t/local-date 2016 3 2)}
                  (first retrieved))
      "The price is created in the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (assert-not-found response)
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
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        commodity (find-commodity ctx "AAPL")
        response (-> (req/request :get (str (path :api
                                                  :commodities
                                                  (:id commodity)
                                                  :prices)
                                            "?"
                                            (map->query-string {:start-date "2016-01-01"
                                                                :end-date "2016-12-31"})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (assert-successful response)
  (is (= [{:trade-date "2016-03-02"
           :price 11.0}
          {:trade-date "2016-02-27"
           :price 10.0}]
         (map #(select-keys % [:trade-date
                               :price])
              body))))

(defn- assert-blocked-list
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-a-list-of-prices-from-his-entity
  (assert-successful-list (get-a-list-by-commodity "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-prices-from-anothers-entity
  (assert-blocked-list (get-a-list-by-commodity "jane@doe.com")))

(defn- update-a-price
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        price (find-price ctx "AAPL" (t/local-date 2016 2 27))
        response (-> (req/request :patch (path :api
                                                :prices
                                                (serialize-date (:trade-date price))
                                                (:id price)))
                     (req/json-body {:trade-date "2016-02-27"
                                     :price 9.99})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (prices/find-by-id (env :db) (:id price) (:trade-date price))]
    [response body retrieved]))

(defn- assert-successful-update
  [[response body retrieved]]
  (assert-successful response)
  (is (selective= {:trade-date "2016-02-27"
                   :price 9.99}
                  body)
      "The response contains the updated price")
  (is (selective= {:trade-date (t/local-date 2016 2 27)
                   :price 9.99M}
                  retrieved)
      "The database record is updated"))

(defn- assert-blocked-update
  [[response _ retrieved]]
  (assert-not-found response)
  (is (selective= {:trade-date (t/local-date 2016 2 27)
                   :price 10M}
                  retrieved)
      "The database record is not updated"))

(deftest a-user-can-update-a-price-for-a-commodity-is-his-entity
  (assert-successful-update (update-a-price "john@doe.com")))

(deftest a-user-cannot-update-a-price-for-a-commodity-in-anothers-entity
  (assert-blocked-update (update-a-price "jane@doe.com")))

(defn- delete-a-price
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        price (find-price ctx "AAPL" (t/local-date 2016 2 27))
        response (-> (req/request :delete (path :api
                                                :prices
                                                (serialize-date (:trade-date price))
                                                (:id price)))
                     (add-auth user)
                     app)
        retrieved (prices/find-by-id (env :db) (:id price) (:trade-date price))]
    [response retrieved]))

(defn- assert-successful-delete
  [[response retrieved]]
  (assert-successful response)
  (is (nil? retrieved) "The price cannot be retrieved after delete."))

(defn- assert-blocked-delete
  [[response retrieved]]
  (assert-not-found response)
  (is retrieved "The price can still be retrieved after blocked delete."))

(deftest a-user-can-delete-a-price-in-his-entity
  (assert-successful-delete (delete-a-price "john@doe.com")))

(deftest a-user-cannot-delete-a-price-in-anothers-entity
  (assert-blocked-delete (delete-a-price "jane@doe.com")))
