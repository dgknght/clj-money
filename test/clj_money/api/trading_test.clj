(ns clj-money.api.trading-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.test-context :refer [realize
                                            find-entity
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.models.transactions :as trans]
            [clj-money.models.lots :as lots]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private buy-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :commodities [{:symbol "AAPL"
                  :name "Apple, Inc."
                  :type :stock
                  :exchange :nasdaq}
                 {:symbol "USD"
                  :name "US Dollar"
                  :type :currency}]
   :accounts [{:name "IRA"
               :type :asset
               :entity-id "Personal"
               :currency-id "USD"}
              {:name "Opening Balances"
               :type :equity
               :entity-id "Personal"
               :currency-id "USD"}]
   :transactions [{:transaction-date (t/local-date 2016 1 1)
                   :description "Opening balances"
                   :debit-account-id "IRA"
                   :credit-account-id "Opening Balances"
                   :quantity 1000M}]})

(defn- buy-a-commodity
  [email]
  (let [context (realize buy-context)
        entity (find-entity context "Personal")
        aapl (find-commodity context "AAPL")
        ira (find-account context "IRA")
        user (find-user context email)
        attr {:trade-date (t/local-date 2016 3 2)
              :action :buy
              :entity-id (:id entity)
              :shares 100.0M
              :value 1000.0M
              :commodity-id (:id aapl)
              :account-id (:id ira)}
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :trades))
                     (edn-body attr)
                     (add-auth user)
                     app
                     parse-edn-body)
        transactions (trans/search {:entity-id (:id entity)
                                    :transaction-date (t/local-date 2016 3 2)})]
    [response transactions]))

(defn- assert-successful-purchase
  [[{:as response :keys [edn-body]} transactions]]
  (is (http-success? response))
  (let [expected {:transaction-date (t/local-date 2016 3 2)
                  :description "Purchase 100.0 shares of AAPL at 10.000"}]
    (is (comparable? expected
                     (:transaction edn-body))
        "The creating transaction is returned in the response")
    (is (seq-with-map-like? expected transactions)
        "The new transaction can be retrieved from the database")))

(defn- assert-blocked-purchase
  [[response _ transactions]]
  (is (http-not-found? response))
  (is (seq-with-no-map-like? {:transaction-date (t/local-date 2016 3 2)
                              :description "Purchase 100.0 shares of AAPL at 10.000"}
                             transactions)
      "The transaction is not created"))

(deftest a-user-can-purchase-a-commodity-in-his-entity
  (assert-successful-purchase (buy-a-commodity "john@doe.com")))

(deftest a-user-cannot-purchase-a-commodity-in-anothers-entity
  (assert-blocked-purchase (buy-a-commodity "jane@doe.com")))

{:users [(factory :user {:email "john@doe.com"})
         (factory :user {:email "jane@doe.com"})]
 :entities [{:name "Personal"
             :user-id "john@doe.com"}]
 :commodities [{:symbol "AAPL"
                :name "Apple, Inc."
                :type :stock
                :exchange :nasdaq}
               {:symbol "USD"
                :name "US Dollar"
                :type :currency}]
 :accounts [{:name "IRA"
             :type :asset
             :entity-id "Personal"
             :currency-id "USD"}
            {:name "Opening Balances"
             :type :equity
             :entity-id "Personal"
             :currency-id "USD"}]
 :transactions [{:transaction-date (t/local-date 2016 1 1)
                 :debit-account-id "IRA"
                 :credit-account-id "Opening Balances"
                 :quantity 1000M}]}
(def ^:private sell-context
  (assoc buy-context
         :trades [{:entity-id "Personal"
                   :trade-date (t/local-date 2016 2 27)
                   :type :purchase
                   :account-id "IRA"
                   :commodity-id "AAPL"
                   :shares 100M
                   :value 1000M}]))

(defn- sell-a-commodity
  [email]
  (let [ctx (realize sell-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        aapl (find-commodity ctx "AAPL")
        ira (find-account ctx "IRA")
        attr {:trade-date (t/local-date 2016 3 2)
              :action :sell
              :entity-id (:id entity)
              :shares 100M
              :value 1100M
              :commodity-id (:id aapl)
              :account-id (:id ira)}
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :trades))
                     (edn-body attr)
                     (add-auth user)
                     app
                     parse-edn-body)
        transactions (trans/search {:entity-id (:id entity)
                                    :transaction-date (t/local-date 2016 3 2)})
        lots (lots/search  {:account-id (:id ira)
                            :commodity-id (:id aapl)})]
    [response transactions lots]))

(defn- assert-successful-sale
  [[{:as response :keys [edn-body]} transactions lots]]
  (is (http-success? response))
  (is (comparable? {:transaction-date (t/local-date 2016 3 2)
                    :description "Sell 100 shares of AAPL at 11.000"}
                   (:transaction edn-body))
      "The created transaction is included in the response")
  (is (seq-with-map-like? {:transaction-date (t/local-date 2016 3 2)
                          :description "Sell 100 shares of AAPL at 11.000"}
                          transactions)
      "The created transaction can be retrieved")
  (is  (= 0M (:shares-owned (first lots)))
       "The shares are not longer owned"))

(defn- assert-blocked-sale
  [[response transactions lots]]
  (is (http-not-found? response))
  (is (seq-with-no-map-like? {:transaction-date (t/local-date 2016 3 2)
                              :description "Sell 100.0 shares of AAPL at 11.000"}
                             transactions)
      "The no transaction is created")
  (is  (= 100M (:shares-owned (first lots)))
      "The shares are still owned"))

(deftest a-user-can-sell-a-commodity-in-his-entity
  (assert-successful-sale (sell-a-commodity "john@doe.com")))

(deftest a-user-cannot-sell-a-commodity-in-anothers-entity
  (assert-blocked-sale (sell-a-commodity "jane@doe.com")))
