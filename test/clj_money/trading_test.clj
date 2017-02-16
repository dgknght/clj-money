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
            [clj-money.trading :as trading]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private purchase-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "IRA"
               :type :asset}
              {:name "Opening balances"
               :type :income}]
   :transactions [{:transaction-date (t/local-date 2017 1 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "IRA"
                            :amount 2000M}
                           {:action :credit
                            :account-id "Opening balances"
                            :amount 2000M}]}]})

(deftest purchase-a-commodity
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts last)
        commodity (-> context :commodities first)
        result (trading/create storage-spec {:commodity-id (:id commodity)
                                               :account-id (:id ira)
                                               :purchase-date (t/local-date 2017 1 2)
                                               :shares 100M
                                               :amount 1000M})]
    (is (:lot result) "The result contains a lot representing the purchased shares")
    (is (:lot-transaction result) "The result contains a lot-transaction")
    (is (:transaction result) "The result contains the transaction associated with the purchase")
    (is (= "Purchase 100 shares of APPL at 10.000" (-> result :transaction :description)) "The transaction description describes the purchase")))

; Purchasing a commodity creates a price record
; A new price causes the account value to change on the home page and in reports
