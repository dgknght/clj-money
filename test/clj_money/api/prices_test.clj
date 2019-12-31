(ns clj-money.api.prices-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-time.core :as t]
            [clj-money.api.test-helper :refer [deftest-create
                                               deftest-delete
                                               deftest-update
                                               deftest-list
                                               deftest-get-one]]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :as h]
            [clj-money.api.prices :as api]))

(def storage-spec (env :db))

(use-fixtures :each (partial h/reset-db storage-spec))

(defn- find-user       [ctx] (h/find-user ctx "john@doe.com"))
(defn- find-other-user [ctx] (h/find-user ctx "jane@doe.com"))

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

(deftest-list list-prices
  {:resource-name "price"
   :context list-context
   :list-fn api/index
   :params-fn (fn [ctx]
                {:commodity-id (:id (h/find-commodity ctx "AAPL"))
                 :trade-date [:between "2016-01-01" "2016-12-31"]})
   :expectation-fn (fn [actual]
                     (let [expected [{:trade-date (t/local-date 2016 2 27)
                                      :price 10M}
                                     {:trade-date (t/local-date 2016 3 2)
                                      :price 11M}]
                           actual (map #(select-keys % [:trade-date
                                                        :price])
                                       actual)]
                       (h/pprint-diff expected actual)
                       (= expected actual)))})
