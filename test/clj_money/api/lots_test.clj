(ns clj-money.api.lots-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-time.core :as t]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

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
                  :exchange :nasdaq
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

(defn- get-lots-for-an-account
  [email]
  (let [ctx (realize list-context)
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
  (is (http-success? response))
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
    (is (= expected actual))))

(defn- assert-blocked-get
  [[response body]]
  (is (http-success? response))
  (is (empty? body) "The body is empty"))

(deftest a-user-can-get-lots-for-an-account-in-his-entity
  (assert-successful-get (get-lots-for-an-account "john@doe.com")))

(deftest a-user-cannot-get-lots-for-an-account-in-anothers-entity
  (assert-blocked-get (get-lots-for-an-account "jane@doe.com")))

(defn- get-lots-for-multiple-accounts
  [email]
  (let [ctx (realize list-context)
        ira (find-account ctx "IRA")
        opening (find-account ctx "Opening Balances")
        user (find-user ctx email)
        response (-> (req/request :get (str (path :api
                                                  :lots)
                                            "?"
                                            (map->query-string {:account-id (map :id [ira opening])})))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(deftest a-user-can-get-lots-for-multiple-accounts-in-his-entity
  (assert-successful-get (get-lots-for-multiple-accounts "john@doe.com")))

(deftest a-user-cannot-get-lots-for-multiple-accounts-in-anothers-entity
  (assert-blocked-get (get-lots-for-multiple-accounts "jane@doe.com")))
