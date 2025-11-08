(ns clj-money.api.trading-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.factories.user-factory]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-user
                                            find-account
                                            find-commodity
                                            find-account]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private transactions
  (some-fn :trade/transaction :transaction))

(def ^:private buy-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:symbol "AAPL"
               :entity "Personal"
               :name "Apple, Inc."
               :type :stock
               :exchange :nasdaq}
   #:commodity{:symbol "USD"
               :entity "Personal"
               :name "US Dollar"
               :type :currency}
   #:account{:name "IRA"
             :type :asset
             :entity "Personal"
             :commodity "USD"}
   #:account{:name "Opening Balances"
             :type :equity
             :entity "Personal"
             :commodity "USD"}
   #:account{:name "Dividends"
             :type :income
             :entity "Personal"
             :commodity "USD"}
   #:transaction{:transaction-date (t/local-date 2016 1 1)
                 :entity "Personal"
                 :description "Opening balances"
                 :debit-account "IRA"
                 :credit-account "Opening Balances"
                 :quantity 1000M}])

(defn- buy-a-commodity
  [email & {:keys [dividend? content-type]
            :or {dividend? false
                 content-type "application/edn"}}]
  (let [entity (find-entity "Personal")
        aapl (find-commodity "AAPL")
        ira (find-account "IRA")
        dividends (find-account "Dividends")
        user (find-user email)
        attr (if (= content-type "application/json")
               (cond-> {:date "2016-03-02"
                        :action "buy"
                        :entity (util/->entity-ref entity)
                        :shares 100
                        :value 1000
                        :commodity (util/->entity-ref aapl)
                        :account (util/->entity-ref ira)
                        :_type "trade"}
                 dividend? (assoc :dividend true
                                  :dividendAccount (util/->entity-ref dividends)))
               (cond-> #:trade{:date (t/local-date 2016 3 2)
                               :action :buy
                               :entity (util/->entity-ref entity)
                               :shares 100M
                               :value 1000M
                               :commodity (util/->entity-ref aapl)
                               :account (util/->entity-ref ira)
                               :dividend? dividend?
                               :dividend-account (util/->entity-ref dividends)}))]
    [(-> (request :post (path :api
                              :entities
                              (:id entity)
                              :trades)
                  :content-type content-type
                  :body attr
                  :user user)
         app
         parse-body)
     (entities/select #:transaction{:entity entity
                                    :transaction-date (t/local-date 2016 3 2)}
                      {:order-by [:transaction/created-at]})]))

(defn- assert-successful-purchase
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected [#:transaction{:transaction-date (t/local-date 2016 3 2)
                                   :description "Purchase 100.000 shares of AAPL at 10.000"}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? (or expected-response expected)
                         (transactions parsed-body))
      "The creating transaction is returned in the response")
  (is (seq-of-maps-like? expected retrieved)
      "The new transaction can be retrieved from the database"))

(defn- assert-successful-reinvestment
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected [#:transaction{:transaction-date (t/local-date 2016 3 2)
                                   :description "Dividend received from AAPL"}
                     #:transaction{:transaction-date (t/local-date 2016 3 2)
                                   :description "Reinvest dividend of 1,000.00: purchase 100.000 shares of AAPL at 10.000"}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? (or expected-response expected)
                         (transactions parsed-body))
      "The new transactions are returned")
  (is (seq-of-maps-like? expected retrieved)
      "The new transactions can be retrieved"))

(defn- assert-blocked-purchase
  [[response retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved)
      "No transactions are not created"))

(deftest a-user-can-purchase-a-commodity-in-his-entity
  (testing "default format"
    (assert-successful-purchase (buy-a-commodity "john@doe.com")))
  (testing "json format"
    (assert-successful-purchase
      (buy-a-commodity "john@doe.com" :content-type "application/json")
      :expected-response [{:transactionDate "2016-03-02"
                           :description "Purchase 100.000 shares of AAPL at 10.000"
                           :_type "transaction"}])))

(deftest a-user-cannot-purchase-a-commodity-in-anothers-entity
  (with-context buy-context
    (assert-blocked-purchase (buy-a-commodity "jane@doe.com"))))

(deftest a-user-can-reinvest-a-dividend-in-his-entity
  (with-context buy-context
    (assert-successful-reinvestment
      (buy-a-commodity "john@doe.com" :dividend? true))))

(deftest a-user-can-reinvest-a-dividend-in-his-entity-with-json
  (with-context buy-context
    (assert-successful-reinvestment
      (buy-a-commodity "john@doe.com" :dividend? true :content-type "application/json")
      :expected-response [{:transactionDate "2016-03-02"
                           :description "Dividend received from AAPL"
                           :_type "transaction"}
                          {:transactionDate "2016-03-02"
                           :description "Reinvest dividend of 1,000.00: purchase 100.000 shares of AAPL at 10.000"
                           :_type "transaction"}])))

(def ^:private sell-context
  (conj buy-context
        #:trade{:entity "Personal"
                :date (t/local-date 2016 2 27)
                :type :purchase
                :account "IRA"
                :commodity "AAPL"
                :shares 100M
                :value 1000M}))

(defn- sell-a-commodity
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [user (find-user email)
        entity (find-entity "Personal")
        aapl (find-commodity "AAPL")
        ira (find-account "IRA")
        attr (if (= content-type "application/json")
               {:date "2016-03-02"
                :action "sell"
                :entity (util/->entity-ref entity)
                :shares 100
                :value 1100
                :commodity (util/->entity-ref aapl)
                :account (util/->entity-ref ira)
                :_type "trade"}
               #:trade{:date (t/local-date 2016 3 2)
                       :action :sell
                       :entity (util/->entity-ref entity)
                       :shares 100M
                       :value 1100M
                       :commodity (util/->entity-ref aapl)
                       :account (util/->entity-ref ira)})]
    [(-> (request :post (path :api
                              :entities
                              (:id entity)
                              :trades)
                  :content-type content-type
                  :body attr
                  :user user)
         app
         parse-body)
     (entities/select #:transaction{:entity entity
                                    :transaction-date (t/local-date 2016 3 2)})
     (entities/find-by #:lot{:account ira
                             :commodity aapl})]))

(defn- assert-successful-sale
  [[{:as response :keys [parsed-body]} trxs lot]
   & {:keys [expected expected-response]
      :or {expected #:transaction{:transaction-date (t/local-date 2016 3 2)
                                  :description "Sell 100.000 shares of AAPL at 11.000"}}}]
  (is (http-success? response))
  (is (seq-of-maps-like? [(or expected-response expected)]
                         (transactions parsed-body))
      "The created transaction is included in the response")
  (is (seq-with-map-like? expected trxs)
      "The created transaction can be retrieved")
  (is  (comparable? #:lot{:shares-owned 0M}
                    lot)
      "The shares are no longer owned"))

(defn- assert-blocked-sale
  [[response transactions lot]]
  (is (http-not-found? response))
  (is (empty? transactions)
      "The no transactions are created")
  (is  (comparable? #:lot{:shares-owned 100M}
                    lot)
      "The shares are still owned"))

(deftest a-user-can-sell-a-commodity-in-his-entity
  (with-context sell-context
    (assert-successful-sale (sell-a-commodity "john@doe.com"))))

(deftest a-user-can-sell-a-commodity-in-his-entity-with-json
  (with-context sell-context
    (assert-successful-sale
      (sell-a-commodity "john@doe.com" :content-type "application/json")
      :expected-response {:transactionDate "2016-03-02"
                          :description "Sell 100.000 shares of AAPL at 11.000"
                          :_type "transaction"})))

(deftest a-user-cannot-sell-a-commodity-in-anothers-entity
  (with-context sell-context
    (assert-blocked-sale (sell-a-commodity "jane@doe.com"))))
