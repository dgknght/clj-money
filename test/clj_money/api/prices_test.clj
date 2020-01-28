(ns clj-money.api.prices-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-time.core :as t]
            [clj-money.x-platform.util :refer [map->query-string
                                               path]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.serialization :as serialization]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-not-found]]
            [clj-money.test-helpers :refer [reset-db
                                            find-user
                                            find-commodity]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each (partial reset-db (env :db)))

(def ^:private list-context
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
                  :type :fund}]
   :prices [{:commodity-id "AAPL"
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
             :price 9.99M}]})

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
                                            (map->query-string {:trade-date [:between "2016-01-01" "2016-12-31"]})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (assert-successful response)
  (is (= [{:trade-date "2016-02-27"
           :price 10.0}
          {:trade-date "2016-03-02"
           :price 11.0}]
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
