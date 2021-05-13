(ns clj-money.api.prices-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path
                                         serialize-date]]
            [dgknght.app-lib.test]
            [clj-time.core :as t]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-price
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
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
                     (req/json-body {:price 12.34
                                     :trade-date "2016-03-02"})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (prices/search {:commodity-id (:id commodity)
                                  :trade-date (t/local-date 2016 3 2)})]
    [response body retrieved]))

(defn- assert-successful-create
  [[response body retrieved]]
  (is (http-success? response))
  (is (comparable? {:price 12.34
                    :trade-date "2016-03-02"}
                   body)
      "The created price is returned in the response")
  (is (comparable? {:price 12.34M
                    :trade-date (t/local-date 2016 3 2)}
                   (first retrieved))
      "The price is created in the database"))

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
        commodity (find-commodity ctx "AAPL")
        response (-> (req/request :get (str (path :api :prices)
                                            "?"
                                            (map->query-string {:start-date "2016-01-01"
                                                                :end-date "2016-12-31"
                                                                :commodity-id (:id commodity)})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (is (http-success? response))
  (is (= [{:trade-date "2016-03-02"
           :price 11.0}
          {:trade-date "2016-02-27"
           :price 10.0}]
         (map #(select-keys % [:trade-date
                               :price])
              body))))

(defn- assert-blocked-list
  [[response body]]
  (is (http-success? response))
  (is (empty? body) "The body is empty"))

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
                                               (serialize-date (:trade-date price))
                                               (:id price)))
                     (req/json-body {:trade-date "2016-02-27"
                                     :price 9.99})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (prices/find price)]
    [response body retrieved]))

(defn- assert-successful-update
  [[response body retrieved]]
  (is (http-success? response))
  (is (comparable? {:trade-date "2016-02-27"
                    :price 9.99}
                   body)
      "The response contains the updated price")
  (is (comparable? {:trade-date (t/local-date 2016 2 27)
                    :price 9.99M}
                   retrieved)
      "The database record is updated"))

(defn- assert-blocked-update
  [[response _ retrieved]]
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
                                                (serialize-date (:trade-date price))
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
