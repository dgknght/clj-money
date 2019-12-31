(ns clj-money.api.trading-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [clj-factory.core :refer [factory]]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-entity
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.serialization :refer [realize]]
            [clj-money.api.trading :as trading]
            [clj-money.models.transactions :as trans]
            [clj-money.models.lots :as lots]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private buy-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "someone@else.com"})
           ]
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
                   :quantity 1000M}]})

(deftest create-a-buy-transaction
  (let [context (realize (env :db) buy-context)
        entity (find-entity context "Personal")
        aapl (find-commodity context "AAPL")
        ira (find-account context "IRA")
        attr {:trade-date "2016-03-02"
              :action :buy
              :entity-id (:id entity)
              :shares 100M
              :value 1000M
              :commodity-id (:id aapl)
              :account-id (:id ira)}]
    (testing "A user cannot create a purchase transaction for someone else's entity"
      (let [user (find-user context "someone@else.com")
            error (try (with-authentication user
                         (trading/create {:params attr}))
                       (catch Exception e
                         e))
            transactions (trans/search (env :db) {:entity-id (:id entity)
                                                  :transaction-date (t/local-date 2016 3 2)})]
        (is (= clojure.lang.ExceptionInfo (type error)) "The expected exception is thrown")
        (is (empty? transactions) "The transaction is not created")))
    (testing "A user can create a purchase transaction for his own entity"
      (let [user (find-user context "john@doe.com")
            response (with-authentication user
                       (trading/create {:params attr}))
            transactions (trans/search (env :db) {:entity-id (:id entity)
                                                  :transaction-date (t/local-date 2016 3 2)})]
        (is (= 201 (:status response)) "The response indicates successful creation")
        (is (= [:transaction :lot] (-> response :body keys))
            "The body contains the transaction and lot that were created")
        (is (= [(t/local-date 2016 3 2)]
               (map :transaction-date transactions))
            "The transaction is created")))))

(def ^:private sell-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "someone@else.com"})
           ]
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
                   :quantity 1000M}]
   :trades [{:entity-id "Personal"
             :trade-date (t/local-date 2016 2 27)
             :type :purchase
             :account-id "IRA"
             :commodity-id "AAPL"
             :shares 100M
             :value 1000M}]})

(deftest create-a-sell-transaction
  (let [context (realize (env :db) sell-context)
        entity (find-entity context "Personal")
        aapl (find-commodity context "AAPL")
        ira (find-account context "IRA")
        attr {:trade-date "2016-03-02"
              :action "sell"; TODO: make this match the serialization :type [:purchase :sale]
              :entity-id (:id entity)
              :shares 100M
              :value 1100M
              :commodity-id (:id aapl)
              :account-id (:id ira)}]
    (testing "A user cannot create a sale transaction for someone else's entity"
      (let [user (find-user context "someone@else.com")
            error (try (with-authentication user
                         (trading/create {:params attr}))
                       (catch Exception e
                         e))
            transactions (trans/search (env :db) {:entity-id (:id entity)
                                                  :transaction-date (t/local-date 2016 3 2)})
            lots (lots/search (env :db) {:account-id (:id ira)
                                         :commodity-id (:id aapl)})]
        (is (= clojure.lang.ExceptionInfo (type error)) "The expected exception is thrown")
        (is (empty? transactions) "The transaction is not created")
        (is  (= 100M (:shares-owned (first lots)))
            "The shares are still owned")))
    (testing "A user can create a sale transaction for his own entity"
      (let [user (find-user context "john@doe.com")
            response (with-authentication user
                       (trading/create {:params attr}))
            transactions (trans/search (env :db) {:entity-id (:id entity)
                                                  :transaction-date (t/local-date 2016 3 2)})
            lots (lots/search (env :db)  {:account-id (:id ira)
                                          :commodity-id (:id aapl)})]
        (is (= 201 (:status response)) "The response indicates successful creation")
        (is (= [:transaction :lots] (-> response :body keys))
            "The body contains the created transaction and affected lots")
        (is (= [(t/local-date 2016 3 2)]
               (map :transaction-date transactions))
            "The transaction is created")
        (is  (= 0M (:shares-owned (first lots)))
            "The shares are not longer owned")))))
