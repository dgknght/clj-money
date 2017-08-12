(ns clj-money.trading-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db
                                            find-account
                                            find-accounts
                                            find-commodity]]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
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
               :type :asset}
              {:name "AAPL"
               :type :asset
               :parent-id "IRA"
               :commodity-id "AAPL"}
              {:name "Opening balances"
               :type :income}
              {:name "Long-term Capital Gains"
               :type :income}
              {:name "Long-term Capital Loss"
               :type :expense}
              {:name "Short-term Capital Gains"
               :type :income}
              {:name "Short-term Capital Loss"
               :type :expense}
              {:name "Investment Expenses"
               :type :expense}
              {:name "Checking"
               :type :asset}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}
                 {:name "Apple, Inc."
                  :symbol "AAPL"
                  :type :stock
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
  {:commodity-id (:id (find-commodity context "AAPL"))
   :account-id (:id (find-account context "IRA"))
   :trade-date (t/local-date 2016 1 2)
   :shares 100M
   :value 1000M})

(deftest purchase-a-commodity
  (let [context (serialization/realize storage-spec purchase-context)
        ira (->> context
                 :accounts
                 (filter #(= "IRA" (:name %)))
                 first)
        apple-account (->> context
                           :accounts
                           (filter #(= "AAPL" (:name %)))
                           first)
        commodity (->> context
                       :commodities
                       (filter #(= "AAPL" (:symbol %)))
                       first)
        result (trading/buy storage-spec (purchase-attributes context))
        expected-transaction {:entity-id (-> context :entities first :id)
                              :transaction-date (t/local-date 2016 1 2)
                              :description "Purchase 100 shares of AAPL at 10.000"
                              :memo nil
                              :items [{:action :credit
                                       :amount 1000M
                                       :balance 1000M
                                       :value 1000M
                                       :account-id (:id ira)
                                       :index 1
                                       :memo nil
                                       :reconciled? false
                                       :reconciliation-id nil}
                                      {:action :debit
                                       :amount 100M
                                       :balance 100M
                                       :value 1000M
                                       :account-id (:id apple-account)
                                       :memo nil
                                       :index 0
                                       :reconciled? false
                                       :reconciliation-id nil}]}
        actual-transaction (-> (:transaction result)
                               (dissoc :updated-at :created-at :id)
                               (update-in [:items]
                                          #(map (fn [i]
                                                  (dissoc i
                                                          :transaction-id
                                                          :updated-at
                                                          :created-at
                                                          :id))
                                                %)))]
    (is (:transaction result)
        "The result contains the transaction associated with the purchase")
    (is (= expected-transaction actual-transaction)
           "The resulting transaction has the correct attributes")
    (is (empty? (-> result :transaction validation/error-messages))
        "The transaction is valid")
    (is (:lot result)
        "The result contains a lot representing the purchased shares")
    (is (empty? (-> result :lot validation/error-messages))
        "The lot is valid")
    (is (empty? (-> result :lot-transaction validation/error-messages))
        "The lot transaction is valud")
    (is (= "Purchase 100 shares of AAPL at 10.000" (-> result :transaction :description)) "The transaction description describes the purchase")))

(deftest purchase-a-commodity-with-string-values
 (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             purchase-attributes
                                             (update-in [:account-id] str)
                                             (update-in [:commodity-id] str)
                                             (update-in [:shares] str)
                                             (update-in [:value] str)
                                             (update-in [:trade-date] str)))]
    (is (empty? (validation/error-messages result))
        "The transaction is valid")))

(deftest purchase-a-commodity-with-a-fee
  (let [context (serialization/realize storage-spec purchase-context)
        ira (->> context
                 :accounts
                 (filter #(= "IRA" (:name %)))
                 first)
        inv-exp (->> context
                     :accounts
                     (filter #(= "Investment Expenses" (:name %)))
                     first)
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             purchase-attributes
                                             (assoc :fee 5M
                                                    :fee-account-id (:id inv-exp))))]
    (is (= 995M (:balance (accounts/reload storage-spec ira)))
        "The investment account balance reflects the fee")
    (is (= 5M (:balance (accounts/reload storage-spec inv-exp)))
        "The investment expense account reflects the fee")))

(deftest purchase-requires-a-commodity-id
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :commodity-id)))]
    (is (= ["Commodity id is required"]
           (validation/error-messages result :commodity-id))
        "The validation message indicates the error")))

(deftest purchase-requires-an-account-id
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :account-id)))]
    (is (= ["Account id is required"]
           (validation/error-messages result :account-id))
        "The validation message indicates the error")))

(deftest purchase-requires-a-trade-date
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :trade-date)))]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The validation message indicates the error")))

(deftest purchase-trade-date-can-be-a-date-string
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (assoc :trade-date "3/2/2016")))]
    (is (empty? (validation/error-messages result))
        "The transaction is valid")))

(deftest purchase-requires-a-number-of-shares
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :shares)))]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The validation message indicates the error")))

(deftest purchase-requires-a-value
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (-> context
                                             (purchase-attributes)
                                             (dissoc :value)))]
    (is (= ["Value is required"]
           (validation/error-messages result :value))
        "The validation message indicates the error")))

(deftest a-purchase-creates-a-lot-record
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
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
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id ira)
                                          :trade-date (t/local-date 2016 1 2)
                                          :shares 100M
                                          :value 1000M})
        expected [{:trade-date (t/local-date 2016 1 2)
                   :action :buy
                   :shares 100M
                   :price 10M
                   :transaction-id (-> result :transaction :id)
                   :lot-id (-> result :lot :id)}]
        actual (map #(dissoc % :id :created-at :updated-at)
                    (lot-transactions/select
                      storage-spec
                      {:lot-id (-> result :lot :id)}))]
    (is (= expected actual)
        "The lot transaction can be retrieved from the database")))

(deftest a-purchase-creates-a-price-record
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
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
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/buy storage-spec {:commodity-id (:id commodity)
                                     :account-id (:id ira)
                                     :trade-date (t/local-date 2016 1 2)
                                     :shares 100M
                                     :value 999M})
        new-balance (->> (accounts/reload storage-spec ira)
                         :balance)]
    (is (= 1001M new-balance) "The account balance decreases by the amount of the purchase")))

(defn- sale-attributes
  [context]
  (let [[ira
         lt-gains
         st-gains
         lt-loss
         st-loss] (find-accounts context
                                 "IRA"
                                 "Long-term Capital Gains"
                                 "Short-term Capital Gains"
                                 "Long-term Capital Loss"
                                 "Short-term Capital Loss")]
    {:commodity-id (:id (find-commodity context "AAPL"))
     :account-id (:id ira)
     :lt-capital-gains-account-id (:id lt-gains)
     :lt-capital-loss-account-id (:id lt-loss)
     :st-capital-gains-account-id (:id st-gains)
     :st-capital-loss-account-id (:id st-loss)
     :inventory-method :fifo
     :trade-date (t/local-date 2017 3 2)
     :shares 25M
     :value 375M}))

(defn- sell-context
  []
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result  (trading/buy storage-spec {:account-id (:id ira)
                                           :commodity-id (:id commodity)
                                           :trade-date (t/local-date 2016 3 2)
                                           :shares 100M
                                           :value 1000M})]
    (assoc context :lots [(:lot result)])))

(deftest sell-a-commodity
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy storage-spec {:account-id (:id ira)
                                            :commodity-id (:id commodity)
                                            :trade-date (t/local-date 2016 3 2)
                                            :shares 100M
                                            :value 1000M})
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
    (is (= 75M (:shares-owned (lots/find-by-id storage-spec (-> purchase :lot :id))))
        "The shares-owned value of the original lot is updated")
    (if (seq (:lot-transactions result))
      (doseq [lot-transaction (:lot-transactions result)]
        (is (empty? (validation/error-messages lot-transaction))
            "Each lot transaction is valid")))
    (is (:transaction result)
        "The result contains the transaction record")
    (is (empty? (-> result :transaction validation/error-messages))
        "The transaction is valid")
    (testing "entity settings"
      (let [expected (select-keys result [:lt-capital-gains-account-id
                                          :st-capital-gains-account-id
                                          :lt-capital-loss-account-id
                                          :st-capital-loss-account-id
                                          :inventory-method])
            actual (->> context
                        :entities
                        first
                        :id
                        (entities/find-by-id storage-spec)
                        :settings)]
        (is (= expected actual)
            "The entity settings are updated with default account ids")))))

(deftest sell-a-commodity-with-a-fee
  (let [context (serialization/realize storage-spec (sell-context))
        ira (find-account context "IRA")
        inv-exp (->> context
                     :accounts
                     (filter #(= "Investment Expenses" (:name %)))
                     first)
        result (trading/sell storage-spec
                             (-> context
                                 sale-attributes
                                 (assoc :fee 5M
                                        :fee-account-id (:id inv-exp))))]
    (is (= 1370M (:balance (accounts/reload storage-spec ira)))
        "The investment account balance reflects the fee")
    (is (= 5M (:balance (accounts/reload storage-spec inv-exp)))
        "The investment fee account balance reflects the fee")))

(deftest sales-requires-an-account-id
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :account-id)))]
    (is (= ["Account id is required"]
           (validation/error-messages result :account-id))
        "The correct validation error is present")))

(deftest sales-requires-a-commodity-id
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :commodity-id)))]
    (is (= ["Commodity id is required"]
           (validation/error-messages result :commodity-id))
        "The correct validation error is present")))

(deftest sales-requires-a-trade-date
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :trade-date)))]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The correct validation error is present")))

(deftest sale-trade-date-can-be-a-date-string
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (assoc :trade-date "3/2/2017")))]
    (is (empty?  (validation/error-messages result))
        "The transaction is value")))

(deftest sales-requires-a-number-of-shares
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :shares)))]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The correct validation error is present")))

(deftest sales-requires-a-value
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :value)))]
    (is (= ["Value is required"]
           (validation/error-messages result :value))
        "The correct validation error is present")))

(deftest sales-requires-a-long-term-capital-gains-account-id
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :lt-capital-gains-account-id)))]
    (is (= ["Lt capital gains account id is required"]
           (validation/error-messages result :lt-capital-gains-account-id))
        "The correct validation error is present")))

(deftest sales-requires-a-long-term-capital-loss-account-id
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :lt-capital-loss-account-id)))]
    (is (= ["Lt capital loss account id is required"]
           (validation/error-messages result :lt-capital-loss-account-id))
        "The correct validation error is present")))

(deftest sales-requires-a-short-term-capital-gains-account-id
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :st-capital-gains-account-id)))]
    (is (= ["St capital gains account id is required"]
           (validation/error-messages result :st-capital-gains-account-id))
        "The correct validation error is present")))

(deftest sales-requires-a-short-term-capital-loss-account-id
  (let [context (serialization/realize storage-spec (sell-context))
        result (trading/sell storage-spec (-> context
                                              sale-attributes
                                              (dissoc :st-capital-loss-account-id)))]
    (is (= ["St capital loss account id is required"]
           (validation/error-messages result :st-capital-loss-account-id))
        "The correct validation error is present")))

(deftest selling-a-commodity-for-a-profit-increases-the-balance-of-the-account
  (let [context (serialization/realize storage-spec purchase-context)
        [ira] (:accounts context)
        commodity (find-commodity context "AAPL")
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
  (let [context (serialization/realize storage-spec (sell-context))
        [ira] (:accounts context)
        commodity (find-commodity context "AAPL")
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
  (let [context (serialization/realize storage-spec (sell-context))
        [ira] (:accounts context)
        commodity (find-commodity context "AAPL")
        lot (-> context :lots first)
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M :value 375M)))
        lot-transactions (map #(dissoc % :id :created-at :updated-at :transaction-id)
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

(deftest selling-a-commodity-for-a-profit-after-1-year-credits-long-term-capital-gains
  (let [context (serialization/realize storage-spec (sell-context))
        ira (find-account context "IRA")
        lt-capital-gains (find-account context "Long-term Capital Gains")
        commodity (find-commodity context "AAPL")
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M :value 375M)))
        gains-items (->> {:account-id (:id lt-capital-gains)}
                         (transactions/search-items storage-spec)
                         (map #(dissoc % :id
                                         :entity-id
                                         :transaction-id
                                         :reconciled?
                                         :reconciliation-id
                                         :created-at
                                         :updated-at)))
        expected [{:transaction-date (t/local-date 2017 3 2)
                   :description "Sell 25 shares of AAPL at 15.000"
                   :action :credit
                   :account-id (:id lt-capital-gains)
                   :amount 125M
                   :value 125M
                   :memo "Sell 25 shares of AAPL at 15.000"
                   :balance 125M
                   :index 0}]]
    (is (= expected gains-items) "The capital gains account is credited the correct amount")))

(deftest selling-a-commodity-for-a-profit-before-1-year-credits-short-term-capital-gains
  (let [context (serialization/realize storage-spec (sell-context))
        ira (find-account context "IRA")
        st-capital-gains (find-account context "Short-term Capital Gains")
        commodity (find-commodity context "AAPL")
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M
                                                :value 375M
                                                :trade-date (t/local-date 2017 3 1))))
        gains-items (->> {:account-id (:id st-capital-gains)}
                         (transactions/search-items storage-spec)
                         (map #(dissoc % :id
                                         :entity-id
                                         :transaction-id
                                         :reconciled?
                                         :reconciliation-id
                                         :created-at
                                         :updated-at)))
        expected [{:transaction-date (t/local-date 2017 3 1)
                   :description "Sell 25 shares of AAPL at 15.000"
                   :action :credit
                   :account-id (:id st-capital-gains)
                   :amount 125M
                   :value 125M
                   :memo "Sell 25 shares of AAPL at 15.000"
                   :balance 125M
                   :index 0}]]
    (is (= expected gains-items) "The capital gains account is credited the correct amount")))

(deftest selling-a-commodity-for-a-loss-debits-capital-loss
  (let [context (sell-context)
        ira (find-account context "IRA")
        capital-loss (find-account context "Long-term Capital Loss")
        commodity (find-commodity context "AAPL")
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 100M :value 850M)))
        gains-items (->> {:account-id (:id capital-loss)}
                         (transactions/search-items storage-spec)
                         (map #(dissoc % :id
                                         :entity-id
                                         :transaction-id
                                         :reconciled?
                                         :reconciliation-id
                                         :created-at
                                         :updated-at)))
        expected [{:transaction-date (t/local-date 2017 3 2)
                   :description "Sell 100 shares of AAPL at 8.500"
                   :action :debit
                   :account-id (:id capital-loss)
                   :amount 150M
                   :value 150M
                   :memo "Sell 100 shares of AAPL at 8.500"
                   :balance 150M
                   :index 0}]]
    (is (= expected gains-items) "The capital loss account is credited the correct amount")))

; Selling a commodity updates a lot record (FILO updates the most recent, FIFO updates the oldest)

(defn- map-accounts
  [context & account-names]
  (map (fn [account-name]
         (->> context
              :accounts
              (filter #(= account-name (:name %)))
              first))
       account-names))

(deftest lifo-sale
  (let [context (serialization/realize storage-spec purchase-context)
        commodity (find-commodity context "AAPL")
        [ira
         lt-gains
         st-gains
         lt-loss
         st-loss] (map-accounts context
                                "IRA"
                                "Long-term Capital Gains"
                                "Short-term Capital Gains"
                                "Long-term Capital Loss"
                                "Short-term Capital Loss")
        _ (trading/buy storage-spec {:trade-date (t/local-date 2015 3 2)
                                          :account-id (:id ira)
                                          :commodity-id (:id commodity)
                                          :shares 100M
                                          :value 1000M})
        _ (trading/buy storage-spec {:trade-date (t/local-date 2016 3 2)
                                          :account-id (:id ira)
                                          :commodity-id (:id commodity)
                                          :shares 100M
                                          :value 2000M})
        sale (trading/sell storage-spec {:trade-date (t/local-date 2017 3 2)
                                      :account-id (:id ira)
                                      :commodity-id (:id commodity)
                                      :shares 50M
                                      :value 1500M
                                      :inventory-method :lifo
                                      :lt-capital-gains-account-id (:id lt-gains)
                                      :st-capital-gains-account-id (:id st-gains)
                                      :lt-capital-loss-account-id (:id lt-loss)
                                      :st-capital-loss-account-id (:id st-loss)})
        actual (->> {:commodity-id (:id commodity)
                     :account-id (:id ira)}
                    (lots/search storage-spec)
                    (sort-by :purchase-date)
                    (map #(dissoc %
                                  :id
                                  :created-at
                                  :updated-at
                                  :commodity-id
                                  :account-id)))
        expected [{:purchase-date (t/local-date 2015 3 2)
                   :shares-purchased 100M
                   :shares-owned 100M}
                  {:purchase-date (t/local-date 2016 3 2)
                   :shares-purchased 100M
                   :shares-owned 50M}]]
    (is (= expected actual) "Shares are sold from the most recent lot")))

(deftest fifo-sale
  (let [context (serialization/realize
                  storage-spec
                  (update-in purchase-context
                             [:entities 0]
                             #(assoc-in % [:settings :inventory-method] :fifo)))
        commodity (find-commodity context "AAPL")
        [ira
         lt-gains
         st-gains
         lt-loss
         st-loss] (map-accounts context
                                "IRA"
                                "Long-term Capital Gains"
                                "Short-term Capital Gains"
                                "Long-term Capital Gains"
                                "Short-term Capital Gains")
        _ (trading/buy storage-spec {:trade-date (t/local-date 2015 3 2)
                                          :account-id (:id ira)
                                          :commodity-id (:id commodity)
                                          :shares 100M
                                          :value 1000M})
        _ (trading/buy storage-spec {:trade-date (t/local-date 2016 3 2)
                                          :account-id (:id ira)
                                          :commodity-id (:id commodity)
                                          :shares 100M
                                          :value 2000M})
        sale (trading/sell storage-spec {:trade-date (t/local-date 2017 3 2)
                                      :account-id (:id ira)
                                      :commodity-id (:id commodity)
                                      :shares 50M
                                      :value 1500M
                                      :lt-capital-gains-account-id (:id lt-gains)
                                      :st-capital-gains-account-id (:id st-gains)
                                      :lt-capital-loss-account-id (:id lt-loss)
                                      :st-capital-loss-account-id (:id st-loss)})
        actual (->> {:commodity-id (:id commodity)
                     :account-id (:id ira)}
                    (lots/search storage-spec)
                    (sort-by :purchase-date)
                    (map #(dissoc %
                                  :id
                                  :created-at
                                  :updated-at
                                  :commodity-id
                                  :account-id)))
        expected [{:purchase-date (t/local-date 2015 3 2)
                   :shares-purchased 100M
                   :shares-owned 50M}
                  {:purchase-date (t/local-date 2016 3 2)
                   :shares-purchased 100M
                   :shares-owned 100M}]]
    (is (= expected actual) "Shares are sold from the most recent lot")))

(deftest undo-a-purchase
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy storage-spec {:trade-date (t/local-date 2017 3 2)
                                            :shares 100M
                                            :commodity-id (:id commodity)
                                            :account-id (:id ira)
                                            :value 1000M})
        result (trading/unbuy storage-spec (-> purchase :transaction :id))]
    ; TODO Should we delete the price that was created?
    (testing "the account balance"
      (is (= 2000M (:balance (accounts/reload storage-spec ira)))
          "The account balance is restored"))
    (testing "the affected lots"
      (is (= [] (lots/search storage-spec {:account-id (:id ira)}))
          "The lot is deleted"))))

(deftest cannot-undo-a-purchase-if-shares-have-been-sold
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy storage-spec {:trade-date (t/local-date 2017 3 2)
                                            :shares 100M
                                            :commodity-id (:id commodity)
                                            :account-id (:id ira)
                                            :value 1000M})
        _ (trading/sell storage-spec (sale-attributes context))]
    (is (thrown-with-msg? IllegalStateException #"Cannot undo"
                          (trading/unbuy storage-spec (-> purchase :transaction :id))))))

(deftest undo-a-sale
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy storage-spec {:trade-date (t/local-date 2017 3 2)
                                            :shares 100M
                                            :commodity-id (:id commodity)
                                            :account-id (:id ira)
                                            :value 1000M})
        sale (trading/sell storage-spec (sale-attributes context))
        result (trading/unsell storage-spec (-> sale :transaction :id))]
    ; IRA balance balance before purchase: $2,000
    ;                      after purchase: $1,000
    ;                          after sale: $1,375
    ;                        after unsale: $1,000
    (testing "the account balance"
      (is (= 1000M (:balance (accounts/reload storage-spec ira)))
          "The account balance is restored"))
    (testing "the lot transactions"
      (doseq [lot-transaction (:lot-transactions sale)]
        (is (not (lot-transactions/find-by-id storage-spec (:id lot-transaction)))
            (format "lot transaction %s should be deleted" (:id lot-transaction)))))
    (testing "the affected lots"
      (doseq [lot (:lots sale)]
        (let [lot (lots/find-by-id storage-spec (:id lot))]
          (is (= (:shares-owned lot) (:shares-purchased lot))
              "The shares-owned should be restored"))))))
