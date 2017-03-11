(ns clj-money.trading-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.lots :as lots]
            [clj-money.models.lot-transactions :as lot-transactions]
            [clj-money.models.prices :as prices]
            [clj-money.trading :as trading]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private purchase-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "IRA"
               :type :asset
               :content-type :commodities}
              {:name "APPL"
               :type :asset
               :parent-id "IRA"
               :content-type :commodity}
              {:name "Opening balances"
               :type :income}]
   :commodities [{:name "Apple, Inc."
                  :symbol "APPL"
                  :exchange :nasdaq}]
   :transactions [{:transaction-date (t/local-date 2016 1 1)
                   :description "Opening balance"
                   :items [{:action :debit
                            :account-id "IRA"
                            :amount 2000M}
                           {:action :credit
                            :account-id "Opening balances"
                            :amount 2000M}]}]})

(deftest purchase-a-commodity
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id ira)
                                          :trade-date (t/local-date 2016 1 2)
                                          :shares 100M
                                          :value 1000M})]
    (is (:transaction result)
        "The result contains the transaction associated with the purchase")
    (is (empty? (-> result :transaction validation/error-messages))
        "The transaction is valid")
    (is (:lot result)
        "The result contains a lot representing the purchased shares")
    (is (empty? (-> result :lot validation/error-messages))
        "The lot is valid")
    (is (:lot-transaction result)
        "The result contains a lot-transaction")
    (is (empty? (-> result :lot-transaction validation/error-messages))
        "The lot transaction is valud")
    (is (= "Purchase 100 shares of APPL at 10.000" (-> result :transaction :description)) "The transaction description describes the purchase")))

(deftest a-purchase-creates-a-lot-record
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec {:commodity-id (:id commodity)
                                     :account-id (:id ira)
                                     :trade-date (t/local-date 2016 1 2)
                                     :shares 100M
                                     :value 1000M})
        expected [{:purchase-date (t/local-date 2016 1 2)
                   :commodity-id (:id commodity)
                   :account-id (:id ira)
                   :shares-purchased 100M
                   :shares-owned 100M}]
        actual (map #(select-keys % [:purchase-date
                                     :commodity-id
                                     :account-id
                                     :shares-purchased
                                     :shares-owned])
                    (lots/select-by-commodity-id storage-spec (:id commodity)))]
    (is (= expected actual) "The lot can be retrieved from the database")))

(deftest a-purchase-creates-a-lot-transaction-recrd
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        _ (trading/buy storage-spec {:commodity-id (:id commodity)
                                     :account-id (:id ira)
                                     :trade-date (t/local-date 2016 1 2)
                                     :shares 100M
                                     :value 1000M})
        expected [{:trade-date (t/local-date 2016 1 2)
                   :action :buy
                   :shares 100M
                   :price 10M
                   :account-id (:id ira)
                   :commodity-id (:id commodity)}]
        actual (map #(select-keys % [:trade-date
                                     :action
                                     :shares
                                     :price
                                     :account-id
                                     :commodity-id])
                    (lot-transactions/select
                      storage-spec
                      {:commodity-id (:id commodity)
                       :account-id (:id ira)}))]
    (is (= expected actual)
        "The lot transaction can be retrieved from the database")))

(deftest a-purchase-creates-a-price-record
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        _ (trading/buy storage-spec {:commodity-id (:id commodity)
                                     :account-id (:id ira)
                                     :trade-date (t/local-date 2016 1 2)
                                     :shares 100M
                                     :value 1000M})
        expected [{:commodity-id (:id commodity)
                   :trade-date (t/local-date 2016 1 2)
                   :price 10M}]
        actual (map #(select-keys % [:commodity-id :trade-date :price])
                    (prices/select-by-commodity-id storage-spec (:id commodity)))]
    (is (= expected actual) "The price can be retrieved from the database")))

(def ^:private sell-context
  (-> purchase-context
      (merge {:lots [{:commodity-id "AAPL"
                      :account-id "IRA"
                      :trade-date (t/local-date 2016 3 2)
                      :shares-purchased 100M
                      :shares-owned 100M}]
              :lot-transactions [{:transaction-date (t/local-date 2016 3 2)
                                  :action :buy
                                  :quantity 100M
                                  :price 10M}]})
      (update-in [:transactions] #(concat % [{:transaction-date 2016 3 2
                                              :description "Purchase 100 shares of APPL at $10.00"
                                              :items [{:action :credit
                                                       :value 1000M
                                                       :account-id "IRA"}
                                                      {:action :debit
                                                       :value 1000M
                                                       :account-id "APPL"}]}]))))

(deftest sell-a-commodity
  (let [context (serialization/realize storage-spec sell-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/sell storage-spec {:commodity-id (:id commodity)
                                           :account-id (:id ira)
                                           :trade-date (t/local-date 2017 3 2)
                                           :shares 25M
                                           :value 375M})]
    (is (:lots result)
        "The result contains the lots affected")
    (is (:lot-transactions result)
        "The result contains a list of lot transactions create by the trade")
    (is (:transaction result)
        "The result contains the transaction record")))

; Selling a commodity updates a lot record (FILO updates the most recent, FIFO updates the oldest)
; Selling a commodity creates a lot transaction record
; Selling a commodity creates a transaction record
; Selling a commodity for a profit credits the capital gains account
; Selling a commodity for a loss debits the capital gains account
; A commodity transaction can have a fee
