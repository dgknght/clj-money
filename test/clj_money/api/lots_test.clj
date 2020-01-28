(ns clj-money.api.lots-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-time.core :as t]
            [clj-money.x-platform.util :refer [path
                                               map->query-string]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-not-found]]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each (partial reset-db (env :db)))

(def ^:private list-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :commodities [{:name "US Dollar"
                  :entity-id "Personal"
                  :symbol "USD"
                  :type :currency}
                 {:name "Some Fund"
                  :entity-id "Personal"
                  :symbol "FND"
                  :type :fund}]
   :accounts [{:name "IRA"
               :entity-id "Personal"
               :commodity-id "USD"
               :type :asset}
              {:name "Opening Balances"
               :entity-id "Personal"
               :commodity-id "USD"
               :type :equity}]
   :transactions [{:transaction-date (t/local-date 2016 1 1)
                   :entity-id "Personal"
                   :description "Opening Balances"
                   :debit-account-id "IRA"
                   :credit-account-id "Opening Balances"
                   :quantity 1000M}]
   :trades [{:type :purchase
             :entity-id "Personal"
             :trade-date (t/local-date 2016 2 1)
             :account-id "IRA"
             :commodity-id "FND"
             :shares 10M
             :value 50M}
            {:type :purchase
             :entity-id "Personal"
             :trade-date (t/local-date 2016 3 1)
             :account-id "IRA"
             :commodity-id "FND"
             :shares 10M
             :value 60M}
            {:type :sale
             :entity-id "Personal"
             :trade-date (t/local-date 2016 3 15)
             :account-id "IRA"
             :commodity-id "FND"
             :shares 5M
             :value 35M}]})

(defn- get-a-list-of-lots
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        account (find-account ctx "IRA")
        commodity (find-commodity ctx "FND")
        user (find-user ctx email)
        response (-> (req/request :get (str (path :api
                                                  :accounts
                                                  (:id account)
                                                  :lots)
                                            "?"
                                            (map->query-string {:commodity-id (:id commodity)})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-get
  [[response body]]
  (assert-successful response)
  (let [expected [{:purchase-date "2016-02-01"
                   :shares-purchased 10.0
                   :purchase-price 5.0
                   :shares-owned 5.0}
                  {:purchase-date "2016-03-01"
                   :shares-purchased 10.0
                   :purchase-price 6.0
                   :shares-owned 10.0}]
        actual (mapv #(select-keys % [:purchase-date
                                      :shares-purchased
                                      :shares-owned
                                      :purchase-price])
                     body)]
    (pprint-diff expected actual)
    (is (= expected actual))))

(defn- assert-blocked-get
  [[response body]]
  (assert-successful response)
  (is (empty? body) "The body is empty"))

(deftest a-user-can-get-lots-from-his-own-entity
  (assert-successful-get (get-a-list-of-lots "john@doe.com")))

(deftest a-user-cannot-get-lots-from-anothers-entity
  (assert-blocked-get (get-a-list-of-lots "jane@doe.com")))
