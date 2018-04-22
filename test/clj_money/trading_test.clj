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
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-commodity
                                            find-transaction
                                            pprint-diff]]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.lots :as lots]
            [clj-money.models.prices :as prices]
            [clj-money.models.transactions :as transactions]
            [clj-money.trading :as trading]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(defn items-by-account
  [account-id]
  (map #(dissoc %
                :id
                :polarized-amount
                :entity-id
                :transaction-id
                :reconciled?
                :reconciliation-id
                :created-at
                :updated-at
                :reconciliation-status)
       (transactions/items-by-account
         storage-spec
         account-id
         [(t/local-date 2015 1 1)
          (t/local-date 2017 12 31)])))

(def ^:private purchase-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "IRA"
               :type :asset}
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
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy storage-spec (purchase-attributes context))
        apple-account (->> {:commodity-id (:id commodity)
                            :entity-id (-> context :entities first :id)}
                           (accounts/search storage-spec)
                           first)
        expected-transaction {:entity-id (-> context :entities first :id)
                              :transaction-date (t/local-date 2016 1 2)
                              :description "Purchase 100 shares of AAPL at 10.000"
                              :memo nil
                              :value 1000M
                              :lot-items [{:lot-action :buy
                                           :shares 100M
                                           :price 10M}]
                              :items [{:action :debit
                                       :amount 100M
                                       :negative false
                                       :polarized-amount 100M
                                       :balance 100M
                                       :value 1000M
                                       :account-id (:id apple-account)
                                       :description "Purchase 100 shares of AAPL at 10.000"
                                       :memo nil
                                       :transaction-date (t/local-date 2016 1 2)
                                       :index 0
                                       :reconciliation-status nil
                                       :reconciled? false
                                       :reconciliation-id nil}
                                      {:action :credit
                                       :amount 1000M
                                       :negative false
                                       :polarized-amount 1000M
                                       :balance 1000M
                                       :value 1000M
                                       :account-id (:id ira)
                                       :description "Purchase 100 shares of AAPL at 10.000"
                                       :index 1
                                       :memo nil
                                       :transaction-date (t/local-date 2016 1 2)
                                       :reconciliation-status nil
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
                                                %))
                               (update-in [:lot-items]
                                          #(map (fn [i]
                                                  (dissoc i :lot-id))
                                                %)))
        expected-commodity-account {:name "AAPL"
                                    :commodity-id (:id commodity)
                                    :entity-id (-> context :entities first :id)
                                    :type :asset
                                    :parent-id (:id ira)
                                    :tags #{:tradable}}
        actual-commodity-account (dissoc apple-account :id
                                                       :created-at
                                                       :updated-at
                                                       :commodity
                                                       :balance)]
    (is (:transaction result)
        "The result contains the transaction associated with the purchase")
    (pprint-diff expected-transaction actual-transaction)
    (is (= expected-transaction actual-transaction)
        "The resulting transaction has the correct attributes")
    (is (empty? (-> result :transaction validation/error-messages))
        "The transaction is valid")
    (is (= "Purchase 100 shares of AAPL at 10.000"
           (-> result :transaction :description))
        "The transaction description describes the purchase")
    (is (:lot result)
        "The result contains a lot representing the purchased shares")
    (is (empty? (-> result :lot validation/error-messages))
        "The lot is valid")
    (is (empty? (-> result :lot-transaction validation/error-messages))
        "The lot transaction is valud")
    (is (= expected-commodity-account
           actual-commodity-account)
        "The commodity account is created")
    (is ((->> ira (accounts/reload storage-spec) :tags) :trading)
        "The specified account is tagged as a trading account")))

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
                    (prices/search storage-spec {:commodity-id (:id commodity)
                                                 :trade-date (t/local-date 2016 1 2)}))]
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
  ([]
   (sell-context purchase-context))
  ([base-context]
   (let [context (serialization/realize storage-spec base-context)
         ira (find-account context "IRA")
         commodity (find-commodity context "AAPL")
         result  (trading/buy storage-spec {:account-id (:id ira)
                                            :commodity-id (:id commodity)
                                            :trade-date (t/local-date 2016 3 2)
                                            :shares 100M
                                            :value 1000M})]
     (-> context
         (assoc :lots [(:lot result)])
         (update-in [:transactions] #(conj % (:transaction result)))))))

(deftest sell-a-commodity
  (let [context (serialization/realize storage-spec purchase-context)
        ira (find-account context "IRA")
        ltcg (find-account context "Long-term Capital Gains")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy storage-spec {:account-id (:id ira)
                                            :commodity-id (:id commodity)
                                            :trade-date (t/local-date 2016 3 2)
                                            :shares 100M
                                            :value 1000M})
        commodity-account (->> {:entity-id (-> context :entities first :id)
                                :commodity-id (:id commodity)}
                               (accounts/search storage-spec)
                               first)
        lot (:lot purchase)
        result (trading/sell storage-spec (sale-attributes context))
        actual-transaction (-> result
                               :transaction
                               (update-in [:items] #(map (fn [i]
                                                           (dissoc i
                                                                   :id
                                                                   :transaction-id
                                                                   :created-at
                                                                   :updated-at))
                                                         %))
                               (dissoc :id :created-at :updated-at))
        expected-transaction {:transaction-date (t/local-date 2017 3 2)
                              :description "Sell 25 shares of AAPL at 15.000"
                              :entity-id (-> context :entities first :id)
                              :memo nil
                              :value 375M
                              :items [{:action :debit
                                       :account-id (:id ira)
                                       :transaction-date (t/local-date 2017 3 2)
                                       :description "Sell 25 shares of AAPL at 15.000"
                                       :amount 375M
                                       :negative false
                                       :polarized-amount 375M
                                       :value 375M
                                       :balance 1375M
                                       :reconciliation-status nil
                                       :reconciled? false
                                       :reconciliation-id nil
                                       :memo nil
                                       :index 2}
                                      {:action :credit
                                       :account-id (:id ltcg)
                                       :memo "Sell 25 shares of AAPL at 15.000"
                                       :transaction-date (t/local-date 2017 3 2)
                                       :description "Sell 25 shares of AAPL at 15.000"
                                       :amount 125M
                                       :negative false
                                       :polarized-amount 125M
                                       :value 125M
                                       :balance 125M
                                       :reconciliation-status nil
                                       :reconciled? false
                                       :reconciliation-id nil
                                       :index 0}
                                      {:action :credit
                                       :account-id (:id commodity-account)
                                       :transaction-date (t/local-date 2017 3 2)
                                       :description "Sell 25 shares of AAPL at 15.000"
                                       :amount 25M
                                       :negative false
                                       :polarized-amount 25M
                                       :balance 75M
                                       :value 250M
                                       :reconciliation-status nil
                                       :reconciled? false
                                       :reconciliation-id nil
                                       :memo nil
                                       :index 1}]
                              :lot-items [{:lot-id (:id lot)
                                           :lot-action :sell
                                           :shares 25M
                                           :price 15M}]}]
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
    (is (= 75M (:shares-owned (lots/find-by-id storage-spec (-> purchase :lot :id))))
        "The shares-owned value of the original lot is updated")
    (is (:transaction result)
        "The result contains the transaction record")
    (pprint-diff expected-transaction actual-transaction)
    (is (= expected-transaction actual-transaction)
        "The transaction contains the correct attributes")
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
        ira (find-account context "IRA")
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
                   :shares-owned 75M
                   :purchase-price 10M}]]
    (is (= expected lots) "The lot is updated to reflect the sale")))

(deftest selling-a-commodity-for-a-profit-after-1-year-credits-long-term-capital-gains
  (let [context (serialization/realize storage-spec (sell-context))
        ira (find-account context "IRA")
        lt-capital-gains (find-account context "Long-term Capital Gains")
        commodity (find-commodity context "AAPL")
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 25M :value 375M)))
        gains-items (items-by-account (:id lt-capital-gains))
        expected [{:transaction-date (t/local-date 2017 3 2)
                   :description "Sell 25 shares of AAPL at 15.000"
                   :action :credit
                   :account-id (:id lt-capital-gains)
                   :amount 125M
                   :negative false
                   :value 125M
                   :memo "Sell 25 shares of AAPL at 15.000"
                   :balance 125M
                   :index 0}]]
    (pprint-diff expected gains-items)
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
        gains-items (items-by-account (:id st-capital-gains))
        expected [{:transaction-date (t/local-date 2017 3 1)
                   :description "Sell 25 shares of AAPL at 15.000"
                   :action :credit
                   :account-id (:id st-capital-gains)
                   :amount 125M
                   :negative false
                   :value 125M
                   :memo "Sell 25 shares of AAPL at 15.000"
                   :balance 125M
                   :index 0}]]
    (pprint-diff expected gains-items)
    (is (= expected gains-items) "The capital gains account is credited the correct amount")))

(deftest selling-a-commodity-for-a-loss-debits-capital-loss
  (let [context (sell-context)
        ira (find-account context "IRA")
        capital-loss (find-account context "Long-term Capital Loss")
        commodity (find-commodity context "AAPL")
        _ (trading/sell storage-spec (-> context
                                         sale-attributes
                                         (assoc :shares 100M :value 850M)))
        gains-items (items-by-account (:id capital-loss))
        expected [{:transaction-date (t/local-date 2017 3 2)
                   :description "Sell 100 shares of AAPL at 8.500"
                   :action :debit
                   :account-id (:id capital-loss)
                   :amount 150M
                   :negative false
                   :value 150M
                   :memo "Sell 100 shares of AAPL at 8.500"
                   :balance 150M
                   :index 0}]]
    (pprint-diff expected gains-items)
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
                   :shares-owned 100M
                   :purchase-price 10M}
                  {:purchase-date (t/local-date 2016 3 2)
                   :shares-purchased 100M
                   :shares-owned 50M
                   :purchase-price 20M}]]
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
                   :shares-owned 50M
                   :purchase-price 10M}
                  {:purchase-date (t/local-date 2016 3 2)
                   :shares-purchased 100M
                   :shares-owned 100M
                   :purchase-price 20M}]]
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
        result (trading/unbuy storage-spec (:transaction purchase))]
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
                          (trading/unbuy storage-spec (:transaction purchase))))))

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
        result (trading/unsell storage-spec (:transaction sale))]
    ; IRA balance balance before purchase: $2,000
    ;                      after purchase: $1,000
    ;                          after sale: $1,375
    ;                        after unsale: $1,000
    (is (= 1000M (:balance (accounts/reload storage-spec ira)))
        "The account balance is restored")
    (testing "the affected lots"
      (doseq [lot (:lots sale)]
        (let [lot (lots/find-by-id storage-spec (:id lot))]
          (is (= (:shares-owned lot) (:shares-purchased lot))
              "The shares-owned should be restored"))))))

(defn- transfer-context []
  (-> purchase-context
      (update-in [:accounts] #(conj % {:name "IRA 2"
                                       :type :asset}))
      sell-context))

(deftest transfer-a-commodity
  (let [context (transfer-context)
        [ira ira-2] (find-accounts context "IRA" "IRA 2")
        commodity (find-commodity context "AAPL")
        result (trading/transfer storage-spec {:commodity-id (:id commodity)
                                               :from-account-id (:id ira)
                                               :to-account-id (:id ira-2)
                                               :shares 100
                                               :transfer-date (t/local-date 2016 4 2)})
        [ira-commodity-account
         ira-2-commodity-account] (->> [ira ira-2]
                                       (map :id)
                                       (map #(accounts/search
                                               storage-spec
                                               {:parent-id %
                                                :commodity-id (:id commodity)}))
                                       (map first))
        entity (find-entity context "Personal")
        actual-lots (map #(dissoc % :created-at :updated-at :id)
                         (lots/search storage-spec {:commodity-id (:id commodity)}))
        expected-lots [{:commodity-id (:id commodity)
                        :account-id (:id ira-2)
                        :shares-owned 100M
                        :purchase-price 10M
                        :shares-purchased 100M
                        :purchase-date (t/local-date 2016 3 2)}]
        expected-transaction {:transaction-date (t/local-date 2016 4 2)
                              :description "Transfer 100 shares of AAPL"
                              :entity-id (:entity-id commodity)
                              :items [{:action :debit
                                       :amount 100M
                                       :value 1000M
                                       :balance 100M
                                       :account-id (:id ira-2-commodity-account)}
                                      {:action :credit
                                       :amount 100M
                                       :value 1000M
                                       :balance 0M
                                       :account-id (:id ira-commodity-account)}]}
        actual-transaction (-> (:transaction result)
                               (select-keys [:entity-id
                                             :transaction-date
                                             :description
                                             :items])
                               (update-in [:items]
                                          (fn [items]
                                            (map #(select-keys
                                                    %
                                                    [:action
                                                     :account-id
                                                     :amount
                                                     :action
                                                     :value
                                                     :balance])
                                                 items))))]
    (is result "A non-nil result is returned")
    (is (empty? (validation/error-messages result))
        "The result does not contain validation errors")
    (pprint-diff expected-transaction actual-transaction)
    (is (= expected-transaction actual-transaction)
        "The correct transaction is returned")
    (pprint-diff expected-lots actual-lots)
    (is (= expected-lots actual-lots)
        "The lots are adjusted correctly.")
    ; Original account balance was 2,000, we bought 1,000 worth of
    ; shares of AAPL, then transfered those shares out of the account
    ; leaving 1,000 in cash
    (is (= 1000M (:balance (accounts/reload storage-spec ira)))
        "The balance in the 'from' account is updated correctly")
    ; No money was ever addedto the second account, so the balance
    ; is still 0
    (is (= 0M (:balance (accounts/reload storage-spec ira-2)))
        "The balance in the 'to' account is updated correclty")))

(def ignorable-item-attributes
  #{:id
    :updated-at
    :created-at
    :index
    :transaction-id
    :negative
    :polarized-amount
    :transaction-date
    :memo
    :reconciliation-id
    :reconciliation-status
    :reconciled?})

(deftest split-a-commodity
  (let [context (sell-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        commodity-account (accounts/find-by storage-spec {:commodity-id (:id commodity)
                                                          :entity-id (:entity-id commodity)})
        result (trading/split storage-spec {:commodity-id (:id commodity)
                                            :account-id (:id ira)
                                            :shares-gained 100M
                                            :split-date (t/local-date 2016 3 2)})
        lots (lots/search storage-spec {:commodity-id (:id commodity)})
        actual-lots (map #(dissoc % :id :created-at :updated-at :commodity-id) lots)
        expected-lots [{:purchase-date (t/local-date 2016 3 2)
                        :account-id (:id ira)
                        :purchase-price 5M
                        :shares-purchased 200M
                        :shares-owned 200M}]
        expected-transaction {:entity-id (:entity-id commodity)
                              :transaction-date (t/local-date 2016 3 2)
                              :description "Split shares of AAPL 2 for 1"
                              :value 0M
                              :items [{:action :debit
                                       :account-id (:id commodity-account)
                                       :amount 100M
                                       :balance 200M
                                       :value 0M
                                       :description "Split shares of AAPL 2 for 1"}]}
        actual-transaction (update-in (dissoc (:transaction result)
                                              :memo
                                              :id
                                              :updated-at
                                              :created-at)
                                      [:items]
                                      #(map (fn [item]
                                              (apply dissoc
                                                     item
                                                     ignorable-item-attributes))
                                            %))]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (= 2M (:ratio result))
        "The correct split ratio is returned")
    (pprint-diff expected-transaction actual-transaction)
    (is (= expected-transaction actual-transaction)
        "The result contains the transaction that was created")
    (pprint-diff expected-lots actual-lots)
    (is (= expected-lots actual-lots)
        "The lots are adjusted correctly")
    ; TODO Need to workout balance of amount vs balance of value
    #_(is (= 200M (:balance (accounts/reload storage-spec ira)))
        "The account has the correct balance after the transfer.")))
