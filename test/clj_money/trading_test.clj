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
            [clj-money.models.accounts :as accounts]
            [clj-money.models.lots :as lots]
            [clj-money.models.lot-transactions :as lot-transactions]
            [clj-money.models.prices :as prices]
            [clj-money.models.transactions :as transactions]
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
               :type :income}
              {:name "Capital Gains"
               :type :income}
              {:name "Capital Loss"
               :type :expense}]
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

(defn- purchase-attributes
  [context]
  {:commodity-id (-> context :commodities first :id)
   :account-id (-> context :accounts first :id)
   :trade-date (t/local-date 2016 1 2)
   :shares 100M
   :value 1000M})

(deftest purchase-a-commodity
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec (purchase-attributes context))]
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

(deftest purchase-requires-a-commodity-id
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :commodity-id)))]
    (is (= ["Commodity id is required"]
           (validation/error-messages result :commodity-id))
        "The validation message indicates the error")))

(deftest purchase-requires-an-account-id
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :account-id)))]
    (is (= ["Account id is required"]
           (validation/error-messages result :account-id))
        "The validation message indicates the error")))

(deftest account-id-must-reference-a-commodities-account
  (is false "need to write the test"))

(deftest purchase-requires-a-trade-date
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :trade-date)))]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The validation message indicates the error")))

(deftest purchase-trade-date-can-be-a-date-string
  (is false "need to write the test"))

(deftest purchase-requires-a-number-of-shares
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :shares)))]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The validation message indicates the error")))

(deftest purchase-requires-a-value
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :value)))]
    (is (= ["Value is required"]
           (validation/error-messages result :value))
        "The validation message indicates the error")))

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
        result (trading/buy storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id ira)
                                          :trade-date (t/local-date 2016 1 2)
                                          :shares 100M
                                          :value 1000M})
        expected [{:trade-date (t/local-date 2016 1 2)
                   :action :buy
                   :shares 100M
                   :price 10M
                   :lot-id (-> result :lot :id)}]
        actual (map #(dissoc % :id :created-at :updated-at)
                    (lot-transactions/select
                      storage-spec
                      {:lot-id (-> result :lot :id)}))]
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

(deftest buying-a-commodity-reduces-the-balance-of-the-account
  (let [context (serialization/realize storage-spec purchase-context)
        ira (-> context :accounts first)
        commodity (-> context :commodities first)
        _ (trading/buy storage-spec {:commodity-id (:id commodity)
                                     :account-id (:id ira)
                                     :trade-date (t/local-date 2016 1 2)
                                     :shares 100M
                                     :value 999M})
        new-balance (->> (accounts/reload storage-spec ira)
                         :balance)]
    (is (= 1001M new-balance) "The account balance decreases by the amount of the purchase")))

(def ^:private sell-context
  (-> purchase-context
      (merge {:lots [{:commodity-id "APPL"
                      :account-id "IRA"
                      :purchase-date (t/local-date 2016 3 2)
                      :shares-purchased 100M
                      :shares-owned 100M}]
              :lot-transactions [{:lot-id {:account-id "IRA"
                                           :commodity-id "APPL"
                                           :purchase-date (t/local-date 2016 3 2)}
                                  :trade-date (t/local-date 2016 3 2)
                                  :action :buy
                                  :shares 100M
                                  :price 10M}]})
      (update-in [:transactions] #(concat % [{:transaction-date 2016 3 2
                                              :description "Purchase 100 shares of APPL at $10.00"
                                              :items [{:action :credit
                                                       :amount 1000M
                                                       :account-id "IRA"}
                                                      {:action :debit
                                                       :amount 1000M
                                                       :account-id "APPL"}]}]))))

(defn- sale-attributes
  [context]
  {:commodity-id (-> context :commodities first :id)
   :account-id (-> context :accounts first :id)
   :capital-gains-account-id (->> context
                                  :accounts
                                  (filter #(= "Capital Gains" (:name %)))
                                  first
                                  :id)
   :capital-loss-account-id (->> context
                                 :accounts
                                 (filter #(= "Capital Loss" (:name %)))
                                 first
                                 :id)
   :trade-date (t/local-date 2017 3 2)
   :shares 25M
   :value 375M})

(deftest sell-a-commodity
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (sale-attributes context))]
    (is (:price result)
        "The result contains a price")
    (is (empty? (-> result :price validation/error-messages))
        "The price is valid")
    (is (:lots result)
        "The result contains the lots affected")
    (if (seq (:lots result))
      (doseq [lot (:lots result)]
        (is (empty? (validation/error-messages lot))
            "Each lot is valid")))
    (is (:lot-transactions result)
        "The result contains a list of lot transactions create by the trade")
    (if (seq (:lot-transactions result))
      (doseq [lot-transaction (:lot-transactions result)]
        (is (empty? (validation/error-messages lot-transaction))
            "Each lot transaction is valid")))
    (is (:transaction result)
        "The result contains the transaction record")
    (is (empty? (-> result :transaction validation/error-messages))
        "The transaction is valid")))

(deftest sales-requires-an-account-id
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :account-id)))]
    (is (= ["Account id is required"]
           (validation/error-messages result :account-id))
        "The correct validation error is present")))

(deftest sale-account-id-must-reference-a-commodities-account
  (is false "need to write the test"))

(deftest sales-requires-a-commodity-id
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :commodity-id)))]
    (is (= ["Commodity id is required"]
           (validation/error-messages result :commodity-id))
        "The correct validation error is present")))

(deftest sales-requires-a-trade-date
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :trade-date)))]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The correct validation error is present")))

(deftest sale-trade-date-can-be-a-date-string
  (is false "need to write the test"))

(deftest sales-requires-a-number-of-shares
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :shares)))]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The correct validation error is present")))

(deftest sales-requires-a-value
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :value)))]
    (is (= ["Value is required"]
           (validation/error-messages result :value))
        "The correct validation error is present")))

(deftest sales-requires-a-capital-gains-account-id
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :capital-gains-account-id)))]
    (is (= ["Capital gains account id is required"]
           (validation/error-messages result :capital-gains-account-id))
        "The correct validation error is present")))

(deftest sales-requires-a-capital-loss-account-id
  (let [context (serialization/realize storage-spec sell-context)
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :capital-loss-account-id)))]
    (is (= ["Capital loss account id is required"]
           (validation/error-messages result :capital-loss-account-id))
        "The correct validation error is present")))

(deftest selling-a-commodity-for-a-profit-increases-the-balance-of-the-account
  (let [context (serialization/realize storage-spec purchase-context)
        [ira] (:accounts context)
        commodity (-> context :commodities first)
        _ (trading/buy storage-spec {:commodity-id (:id commodity)
                                     :account-id (:id ira)
                                     :trade-date (t/local-date 2016 1 2)
                                     :shares 100M
                                     :value 1000M})
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (assoc :shares 50M :value 560M)))
        new-balance (->> (accounts/reload storage-spec ira)
                         :balance)]
    (is (= 1560M new-balance) "The account balance decreases by the amount of the purchase")))

(deftest selling-a-commodity-updates-a-lot-record
  (let [context (serialization/realize storage-spec sell-context)
        [ira] (:accounts context)
        commodity (-> context :commodities first)
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M :value 375M)))
        lots (map #(dissoc % :id :created-at :updated-at)
                  (lots/search storage-spec {:account-id (:id ira)
                                             :commodity-id (:id commodity)}))
        expected [{:purchase-date (t/local-date 2016 3 2)
                   :account-id (:id ira)
                   :commodity-id (:id commodity)
                   :shares-purchased 100M
                   :shares-owned 75M}]]
    (is (= expected lots) "The lot is updated to reflect the sale")))

(deftest selling-a-commodity-creates-a-lot-transaction-record
  (let [context (serialization/realize storage-spec sell-context)
        [ira] (:accounts context)
        commodity (-> context :commodities first)
        lot (-> context :lots first)
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M :value 375M)))
        lot-transactions (map #(dissoc % :id :created-at :updated-at)
                              (lot-transactions/select
                                storage-spec
                                {:lot-id (:id lot)}))
        expected [{:trade-date (t/local-date 2016 3 2)
                   :lot-id (:id lot)
                   :action :buy
                   :price 10M
                   :shares 100M}
                  {:trade-date (t/local-date 2017 3 2)
                   :lot-id (:id lot)
                   :action :sell
                   :price 15M
                   :shares 25M}]]
    (is (= expected lot-transactions) "The lot transaction is created with proper data")))

(deftest selling-a-commodity-for-a-profit-credits-capital-gains
  (let [context (serialization/realize storage-spec sell-context)
        [ira _ _ capital-gains] (:accounts context)
        commodity (-> context :commodities first)
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M :value 375M)))
        expected [{:trade-date (t/local-date 2016 3 2)
                   :account-id (:id ira)
                   :commodity-id (:id commodity)
                   :action :buy
                   :price 10M
                   :shares 100M}
                  {:trade-date (t/local-date 2017 3 2)
                   :account-id (:id ira)
                   :commodity-id (:id commodity)
                   :action :sell
                   :price 15M
                   :shares 25M}]
        gains-items (->> {:account-id (:id capital-gains)}
                         (transactions/search-items storage-spec)
                         (map #(dissoc % :id
                                         :entity-id
                                         :transaction-id
                                         :reconciled?
                                         :reconciliation-id
                                         :created-at
                                         :updated-at)))
        expected [{:transaction-date (t/local-date 2017 3 2)
                   :description "Sell 25 shares of APPL at 15.000"
                   :action :credit
                   :account-id (:id capital-gains)
                   :amount 125M
                   :memo "Sell 25 shares of APPL at 15.00"
                   :balance 125M
                   :index 0}]]
    (is (= expected gains-items) "The capital gains account is credited the correct amount")))

; Selling a commodity updates a lot record (FILO updates the most recent, FIFO updates the oldest)
; Selling a commodity creates a transaction record
; Selling a commodity for a loss debits the capital gains account
; A commodity transaction can have a fee
