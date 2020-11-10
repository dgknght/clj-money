(ns clj-money.trading-test
  (:require [clojure.test :refer [use-fixtures deftest is testing]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.util :refer [->id]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db
                                            selective=]]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.lots :as lots]
            [clj-money.models.prices :as prices]
            [clj-money.models.transactions :as transactions]
            [clj-money.trading :as trading]))

(use-fixtures :each reset-db)

(defn- items-by-account
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
         account-id
         [(t/local-date 2015 1 1)
          (t/local-date 2017 12 31)])))

(defn- item-by-account
  [acc-or-id transaction]
  {:pre [acc-or-id transaction]}

  (let [id (->id acc-or-id)]
    (->> (:items transaction)
         (filter #(= id (:account-id %)))
         first)))

(def ^:private purchase-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "IRA"
               :type :asset}
              {:name "Opening balances"
               :type :income}
              {:name "Long-term Capital Gains"
               :type :income}
              {:name "Long-term Capital Losses"
               :type :expense}
              {:name "Short-term Capital Gains"
               :type :income}
              {:name "Short-term Capital Losses"
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
                            :quantity 2000M}
                           {:action :credit
                            :account-id "Opening balances"
                            :quantity 2000M}]}]})

(defn- purchase-attributes
  [context]
  {:commodity-id (:id (find-commodity context "AAPL"))
   :account-id (:id (find-account context "IRA"))
   :trade-date (t/local-date 2016 1 2)
   :shares 100M
   :value 1000M})

(deftest purchase-a-commodity
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        result (trading/buy (purchase-attributes context))
        apple-account (accounts/find-by {:commodity-id (:id commodity)
                                         :entity-id (-> context :entities first :id)})
        expected-transaction {:entity-id (-> context :entities first :id)
                              :transaction-date (t/local-date 2016 1 2)
                              :description "Purchase 100 shares of AAPL at 10.000"
                              :memo nil
                              :value 1000M
                              :lot-items [{:lot-action :buy
                                           :transaction-date (t/local-date 2016 1 2)
                                           :shares 100M
                                           :price 10M}]
                              :items [{:action :debit
                                       :quantity 100M
                                       :negative false
                                       :polarized-quantity 100M
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
                                       :quantity 1000M
                                       :negative true
                                       :polarized-quantity -1000M
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
                                    :tags #{:tradable}
                                    :quantity 100M
                                    ; :value 1000M TODO Restore this check
                                    }
        actual-commodity-account (select-keys apple-account [:name
                                                             :commodity-id
                                                             :entity-id
                                                             :type
                                                             :parent-id
                                                             :tags
                                                             :quantity])]
    (is (:transaction result)
        "The result contains the transaction associated with the purchase")
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
    (is ((-> ira accounts/reload :tags) :trading)
        "The specified account is tagged as a trading account")))

(deftest purchase-a-commodity-with-a-fee
  (let [context (realize purchase-context)
        ira (->> context
                 :accounts
                 (filter #(= "IRA" (:name %)))
                 first)
        inv-exp (->> context
                     :accounts
                     (filter #(= "Investment Expenses" (:name %)))
                     first)]
    (trading/buy (-> context
                     purchase-attributes
                     (assoc :fee 5M
                            :fee-account-id (:id inv-exp))))
    (is (= 995M (:quantity (accounts/reload ira)))
        "The investment account balance reflects the fee")
    (is (= 5M (:quantity (accounts/reload inv-exp)))
        "The investment expense account reflects the fee")))

(deftest purchase-requires-a-trade-date
  (let [context (realize purchase-context)
        result (trading/buy  (-> context
                                 (purchase-attributes)
                                 (dissoc :trade-date)))]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The validation message indicates the error")))

(deftest purchase-requires-a-number-of-shares
  (let [context (realize purchase-context)
        result (trading/buy (-> context
                                (purchase-attributes)
                                (dissoc :shares)))]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The validation message indicates the error")))

(deftest purchase-requires-a-value
  (let [context (realize purchase-context)
        result (trading/buy (-> context
                                (purchase-attributes)
                                (dissoc :value)))]
    (is (= ["Value is required"]
           (validation/error-messages result :value))
        "The validation message indicates the error")))

(deftest a-purchase-creates-a-lot-record
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/buy {:commodity-id (:id commodity)
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
                    (lots/select-by-commodity-id (:id commodity)))]
    (is (= expected actual) "The lot can be retrieved from the database")))

(deftest a-purchase-creates-a-price-record
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/buy {:commodity-id (:id commodity)
                        :account-id (:id ira)
                        :trade-date (t/local-date 2016 1 2)
                        :shares 100M
                        :value 1000M})
        expected [{:commodity-id (:id commodity)
                   :trade-date (t/local-date 2016 1 2)
                   :price 10M}]
        actual (map #(select-keys % [:commodity-id :trade-date :price])
                    (prices/search {:commodity-id (:id commodity)
                                    :trade-date (t/local-date 2016 1 2)}))]
    (is (= expected actual) "The price can be retrieved from the database")))

(deftest buying-a-commodity-reduces-the-balance-of-the-account
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/buy {:commodity-id (:id commodity)
                        :account-id (:id ira)
                        :trade-date (t/local-date 2016 1 2)
                        :shares 100M
                        :value 999M})
        new-balance (:quantity (accounts/reload ira))]
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
                                 "Long-term Capital Losses"
                                 "Short-term Capital Losses")]
    {:commodity-id (:id (find-commodity context "AAPL"))
     :account-id (:id ira)
     :lt-capital-gains-account-id (:id lt-gains)
     :lt-capital-loss-account-id (:id lt-loss)
     :st-capital-gains-account-id (:id st-gains)
     :st-capital-loss-account-id (:id st-loss)
     :inventory-method :fifo
     :trade-date (t/local-date 2017 3 2)
     :shares 25M
     :value 375M})) ; Sell at $15/share or $125 gain

(def ^:private sale-context
  (assoc purchase-context
         :trades [{:type :purchase
                   :account-id "IRA"
                   :commodity-id "AAPL"
                   :trade-date (t/local-date 2016 3 2)
                   :shares 100M
                   :value 1000M}]))

(deftest sell-a-commodity-for-a-gain
  (let [context (realize sale-context)
        entity (find-entity context "Personal")
        aapl (find-commodity context "AAPL")
        {:keys [transaction
                price
                lots]
         :as result} (trading/sell (sale-attributes context))
        ira (find-account context "IRA")
        ltcg (find-account context "Long-term Capital Gains")
        aapl-acc (accounts/find-by {:entity-id (:id entity)
                                    :commodity-id (:id aapl)})
        lot (lots/find-by {:commodity-id (:id aapl)
                           :account-id (:id ira)
                           :purchase-date (t/local-date 2016 3 2)})]
    (is price "The result contains a price")
    (is (empty? (validation/error-messages price ))
        "The price is valid")
    (is lots "The result contains the lots affected")
    (doseq [lot lots]
      (is (empty? (validation/error-messages lot))
          "Each lot is valid"))
    (is (= 75M (:shares-owned lot))
        "The shares-owned value of the original lot is updated")
    (is transaction "The result contains the transaction record")
    (is (empty? (validation/error-messages transaction ))
        "The transaction is valid")
    (is (selective= {:action :debit
                     :value 375M
                     :quantity 375M}
                    (item-by-account ira transaction))
        "The trading account is debited the total proceeds from the purchase")
    (is (selective= {:action :credit
                     :value 125M
                     :quantity 125M}
                    (item-by-account ltcg transaction))
        "The capital gains account is credited the amount received above the original cost of the shares.")
    (is (selective= {:action :credit
                     :value 250M
                     :quantity 25M}
                    (item-by-account aapl-acc transaction))
        "The commodity account is credited the number of shares and purchase value of the shares.")
    (testing "entity settings"
      (let [expected (select-keys result [:lt-capital-gains-account-id
                                          :st-capital-gains-account-id
                                          :lt-capital-loss-account-id
                                          :st-capital-loss-account-id
                                          :inventory-method])
            actual (-> (entities/find entity)
                       :settings
                       (select-keys [:lt-capital-gains-account-id
                                     :st-capital-gains-account-id
                                     :lt-capital-loss-account-id
                                     :st-capital-loss-account-id
                                     :inventory-method]))]
        (is (= expected actual)
            "The entity settings are updated with default account ids")))))

(deftest sell-a-commodity-for-a-loss
  (let [context (realize sale-context)
        entity (find-entity context "Personal")
        aapl (find-commodity context "AAPL")
        attr (assoc (sale-attributes context) :value 200M) ; 25 shares, $50 loss
        {:keys [lots transaction price]} (trading/sell attr)
        ira (find-account context "IRA")
        ltcl (find-account context "Long-term Capital Losses")
        aapl-acc (accounts/find-by {:entity-id (:id entity)
                                    :commodity-id (:id aapl)})
        lot (lots/find-by {:commodity-id (:id aapl)
                           :account-id (:id ira)
                           :purchase-date (t/local-date 2016 3 2)})]
    (is (= 8M (:price price))
        "The result contains the correct price")
    (is (empty? (validation/error-messages price))
        "The price is valid")
    (is lots "The result contains the lots affected")
    (doseq [lot lots]
      (is (empty? (validation/error-messages lot))
          "Each lot is valid"))
    (is (= 75M (:shares-owned lot))
        "The shares-owned value of the original lot is updated")
    (is transaction "The result contains the transaction record")
    (is (empty? (validation/error-messages transaction))
        "The transaction is valid")
    (is (selective= {:action :debit
                     :value 200M
                     :quantity 200M}
                    (item-by-account ira transaction))
        "The trading account is debited the total proceeds from the purchase")
    (is (selective= {:action :debit
                     :value 50M
                     :quantity 50M}
                    (item-by-account ltcl transaction))
        "The capital loss account is debited the cost the shares less the sale proceeds")
    (is (selective= {:action :credit
                     :value 250M
                     :quantity 25M}
                    (item-by-account aapl-acc transaction))
        "The commodity account is credited the number of shares and purchase value of the shares.")))

(def ^:private auto-create-context
  (update-in sale-context [:accounts] (fn [accounts]
                                        (remove #(re-find #"Capital" (:name %)) accounts))))

(deftest auto-create-gains-accounts
  (let [context (realize auto-create-context)
        _ (trading/sell (sale-attributes context))
        entity (entities/find (find-entity context "Personal"))
        ltcg (accounts/find-by {:entity-id (:id entity)
                                :name "Long-term Capital Gains"})
        stcg (accounts/find-by {:entity-id (:id entity)
                                :name "Short-term Capital Gains"})
        ltcl (accounts/find-by {:entity-id (:id entity)
                                :name "Long-term Capital Losses"})
        stcl (accounts/find-by {:entity-id (:id entity)
                                :name "Short-term Capital Losses"})]
    (is (selective= {:type :income}
                    ltcg)
        "The long-term capital gains account is an income account")
    (is (= (get-in entity [:settings :lt-capital-gains-account-id])
           (:id ltcg))
        "The Long-term Capital Gains account id is placed in the entity settings")
    (is (selective= {:type :income}
                    stcg)
        "The short-term capital gains account is an income account")
    (is (= (get-in entity [:settings :st-capital-gains-account-id])
           (:id stcg))
        "The Short-term Capital Gains account id is placed in the entity settings")
    (is (selective= {:type :expense}
                    ltcl)
        "The long-term capital losses account is an expense account")
    (is (= (get-in entity [:settings :lt-capital-loss-account-id])
           (:id ltcl))
        "The Long-term Capital Losses account id is placed in the entity settings")
    (is (selective= {:type :expense}
                    stcl)
        "The short-term capital losses account is an expense account")
    (is (= (get-in entity [:settings :st-capital-loss-account-id])
           (:id stcl))
        "The Short-term Capital Losses account id is placed in the entity settings")))

(deftest sell-a-commodity-with-a-fee
  (let [context (realize sale-context)
        ira (find-account context "IRA")
        inv-exp (->> context
                     :accounts
                     (filter #(= "Investment Expenses" (:name %)))
                     first)]
    (trading/sell (-> context
                      sale-attributes
                      (assoc :fee 5M
                             :fee-account-id (:id inv-exp))))
    (is (= 1370M (:quantity (accounts/reload ira)))
        "The investment account balance reflects the fee")
    (is (= 5M (:quantity (accounts/reload inv-exp)))
        "The investment fee account balance reflects the fee")))

(deftest sales-requires-a-trade-date
  (let [context (realize sale-context)
        result (trading/sell (-> context
                                 sale-attributes
                                 (dissoc :trade-date)))]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The correct validation error is present")))

(deftest sales-requires-a-number-of-shares
  (let [context (realize sale-context)
        result (trading/sell (-> context
                                 sale-attributes
                                 (dissoc :shares)))]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The correct validation error is present")))

(deftest sales-requires-a-value
  (let [context (realize sale-context)
        result (trading/sell (-> context
                                 sale-attributes
                                 (dissoc :value)))]
    (is (= ["Value is required"]
           (validation/error-messages result :value))
        "The correct validation error is present")))

(deftest selling-a-commodity-for-a-profit-increases-the-balance-of-the-account
  (let [context (realize purchase-context)
        [ira] (:accounts context)
        commodity (find-commodity context "AAPL")
        _ (trading/buy {:commodity-id (:id commodity)
                        :account-id (:id ira)
                        :trade-date (t/local-date 2016 1 2)
                        :shares 100M
                        :value 1000M})
        _ (trading/sell (-> context
                            sale-attributes
                            (assoc :shares 50M :value 560M)))
        new-balance (:quantity (accounts/reload ira))]
    (is (= 1560M new-balance) "The account balance decreases by the amount of the purchase")))

(deftest selling-a-commodity-updates-a-lot-record
  (let [context (realize sale-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/sell (-> context
                            sale-attributes
                            (assoc :shares 25M :value 375M)))
        lots (map #(dissoc % :id :created-at :updated-at)
                  (lots/search {:account-id (:id ira)
                                :commodity-id (:id commodity)}))
        expected [{:purchase-date (t/local-date 2016 3 2)
                   :account-id (:id ira)
                   :commodity-id (:id commodity)
                   :shares-purchased 100M
                   :shares-owned 75M
                   :purchase-price 10M}]]
    (is (= expected lots) "The lot is updated to reflect the sale")))

(deftest selling-a-commodity-for-a-profit-after-1-year-credits-long-term-capital-gains
  (let [context (realize sale-context)
        lt-capital-gains (find-account context "Long-term Capital Gains")
        _ (trading/sell (-> context
                            sale-attributes
                            (assoc :shares 25M :value 375M)))
        gains-items (items-by-account (:id lt-capital-gains))
        expected [{:transaction-date (t/local-date 2017 3 2)
                   :description "Sell 25 shares of AAPL at 15.000"
                   :action :credit
                   :account-id (:id lt-capital-gains)
                   :quantity 125M
                   :polarized-quantity 125M
                   :negative false
                   :value 125M
                   :memo "Sell 25 shares of AAPL at 15.000"
                   :balance 125M
                   :index 0}]]
    (is (= expected gains-items) "The capital gains account is credited the correct amount")))

(deftest selling-a-commodity-for-a-profit-before-1-year-credits-short-term-capital-gains
  (let [context (realize sale-context)
        st-capital-gains (find-account context "Short-term Capital Gains")
        _ (trading/sell (-> context
                            sale-attributes
                            (assoc :shares 25M
                                   :value 375M
                                   :trade-date (t/local-date 2017 3 1))))
        gains-items (items-by-account (:id st-capital-gains))
        expected [{:transaction-date (t/local-date 2017 3 1)
                   :description "Sell 25 shares of AAPL at 15.000"
                   :action :credit
                   :account-id (:id st-capital-gains)
                   :quantity 125M
                   :polarized-quantity 125M
                   :negative false
                   :value 125M
                   :memo "Sell 25 shares of AAPL at 15.000"
                   :balance 125M
                   :index 0}]]
    (is (= expected gains-items) "The capital gains account is credited the correct amount")))

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
  (let [context (realize purchase-context)
        commodity (find-commodity context "AAPL")
        [ira
         lt-gains
         st-gains
         lt-loss
         st-loss] (map-accounts context
                                "IRA"
                                "Long-term Capital Gains"
                                "Short-term Capital Gains"
                                "Long-term Capital Losses"
                                "Short-term Capital Losses")
        _ (trading/buy {:trade-date (t/local-date 2015 3 2)
                        :account-id (:id ira)
                        :commodity-id (:id commodity)
                        :shares 100M
                        :value 1000M})
        _ (trading/buy {:trade-date (t/local-date 2016 3 2)
                        :account-id (:id ira)
                        :commodity-id (:id commodity)
                        :shares 100M
                        :value 2000M})
        _ (trading/sell {:trade-date (t/local-date 2017 3 2)
                         :account-id (:id ira)
                         :commodity-id (:id commodity)
                         :shares 50M
                         :value 1500M
                         :inventory-method :lifo
                         :lt-capital-gains-account-id (:id lt-gains)
                         :st-capital-gains-account-id (:id st-gains)
                         :lt-capital-loss-account-id (:id lt-loss)
                         :st-capital-loss-account-id (:id st-loss)})
        actual (->> (lots/search {:commodity-id (:id commodity)
                                  :account-id (:id ira)})
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
  (let [context (realize
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
        _ (trading/buy {:trade-date (t/local-date 2015 3 2)
                        :account-id (:id ira)
                        :commodity-id (:id commodity)
                        :shares 100M
                        :value 1000M})
        _ (trading/buy {:trade-date (t/local-date 2016 3 2)
                        :account-id (:id ira)
                        :commodity-id (:id commodity)
                        :shares 100M
                        :value 2000M})
        _ (trading/sell {:trade-date (t/local-date 2017 3 2)
                         :account-id (:id ira)
                         :commodity-id (:id commodity)
                         :shares 50M
                         :value 1500M
                         :lt-capital-gains-account-id (:id lt-gains)
                         :st-capital-gains-account-id (:id st-gains)
                         :lt-capital-loss-account-id (:id lt-loss)
                         :st-capital-loss-account-id (:id st-loss)})
        actual (->> (lots/search {:commodity-id (:id commodity)
                                  :account-id (:id ira)})
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
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy {:trade-date (t/local-date 2017 3 2)
                               :shares 100M
                               :commodity-id (:id commodity)
                               :account-id (:id ira)
                               :value 1000M})]
    (trading/unbuy (:transaction purchase))
    ; TODO Should we delete the price that was created?
    (testing "the account balance"
      (is (= 2000M (:quantity (accounts/reload ira)))
          "The account balance is restored"))
    (testing "the affected lots"
      (is (= [] (lots/search {:account-id (:id ira)}))
          "The lot is deleted"))))

(deftest cannot-undo-a-purchase-if-shares-have-been-sold
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        purchase (trading/buy {:trade-date (t/local-date 2017 3 2)
                               :shares 100M
                               :commodity-id (:id commodity)
                               :account-id (:id ira)
                               :value 1000M})
        _ (trading/sell (sale-attributes context))]
    (is (thrown-with-msg? IllegalStateException #"Cannot undo"
                          (trading/unbuy (:transaction purchase))))))

(deftest undo-a-sale
  (let [context (realize purchase-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        _ (trading/buy {:trade-date (t/local-date 2017 3 2)
                        :shares 100M
                        :commodity-id (:id commodity)
                        :account-id (:id ira)
                        :value 1000M})
        sale (trading/sell (sale-attributes context))
        _ (trading/unsell (:transaction sale))]
    ; IRA balance balance before purchase: $2,000
    ;                      after purchase: $1,000
    ;                          after sale: $1,375
    ;                        after unsale: $1,000
    (is (= 1000M (:quantity (accounts/reload ira)))
        "The account balance is restored")
    (testing "the affected lots"
      (doseq [lot (map lots/find (:lots sale))]
        (is (= (:shares-owned lot) (:shares-purchased lot))
            "The shares-owned should be restored")))))

(def ^:private transfer-context
  (update-in sale-context [:accounts] conj {:name "IRA 2"
                                            :type :asset}))

(deftest transfer-a-commodity
  (let [context (realize transfer-context)
        [ira ira-2] (find-accounts context "IRA" "IRA 2")
        commodity (find-commodity context "AAPL")
        result (trading/transfer {:commodity-id (:id commodity)
                                  :from-account-id (:id ira)
                                  :to-account-id (:id ira-2)
                                  :shares 100M
                                  :transfer-date (t/local-date 2016 4 2)})
        [ira-commodity-account
         ira-2-commodity-account] (->> [ira ira-2]
                                       (map :id)
                                       (map #(accounts/search
                                               {:parent-id %
                                                :commodity-id (:id commodity)}))
                                       (map first))
        actual-lots (map #(dissoc % :created-at :updated-at :id)
                         (lots/search {:commodity-id (:id commodity)}))
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
                                       :quantity 100M
                                       :value 1000M
                                       :balance 100M
                                       :account-id (:id ira-2-commodity-account)}
                                      {:action :credit
                                       :quantity 100M
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
                                                     :quantity
                                                     :action
                                                     :value
                                                     :balance])
                                                 items))))]
    (is result "A non-nil result is returned")
    (is (empty? (validation/error-messages result))
        "The result does not contain validation errors")
    (is (= expected-transaction actual-transaction)
        "The correct transaction is returned")
    (is (= expected-lots actual-lots)
        "The lots are adjusted correctly.")
    ; Original account balance was 2,000, we bought 1,000 worth of
    ; shares of AAPL, then transfered those shares out of the account
    ; leaving 1,000 in cash
    (is (= 1000M (:quantity (accounts/reload ira)))
        "The balance in the 'from' account is updated correctly")
    ; No money was ever addedto the second account, so the balance
    ; is still 0
    (is (= 0M (:quantity (accounts/reload ira-2)))
        "The balance in the 'to' account is updated correclty")))

(deftest split-a-commodity
  (let [context (realize sale-context)
        ira (find-account context "IRA")
        commodity (find-commodity context "AAPL")
        commodity-account (accounts/find-by {:commodity-id (:id commodity)
                                             :entity-id (:entity-id commodity)})
        result (trading/split {:commodity-id (:id commodity)
                               :account-id (:id ira)
                               :shares-gained 100M
                               :split-date (t/local-date 2016 3 3)})
        lots (lots/search {:commodity-id (:id commodity)})
        actual-lots (map #(dissoc % :id :created-at :updated-at :commodity-id) lots)
        expected-lots [{:purchase-date (t/local-date 2016 3 2)
                        :account-id (:id ira)
                        :purchase-price 5M
                        :shares-purchased 200M
                        :shares-owned 200M}]
        expected-transaction {:entity-id (:entity-id commodity)
                              :transaction-date (t/local-date 2016 3 3)
                              :description "Split shares of AAPL 2 for 1"
                              :value 0M
                              :items [{:action :debit
                                       :account-id (:id commodity-account)
                                       :quantity 100M
                                       :polarized-quantity 100M
                                       :balance 200M
                                       :value 0M
                                       :description "Split shares of AAPL 2 for 1"}]}
        actual-transaction (update-in (dissoc (:transaction result)
                                              :memo
                                              :id
                                              :updated-at
                                              :created-at)
                                      [:items]
                                      #(map (fn [i] (select-keys i [:action
                                                                    :account-id
                                                                    :quantity
                                                                    :polarized-quantity
                                                                    :balance
                                                                    :value
                                                                    :description]))
                                            %))]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (= 2M (:ratio result))
        "The correct split ratio is returned")
    (is (= expected-transaction actual-transaction)
        "The result contains the transaction that was created")
    (is (= expected-lots actual-lots)
        "The lots are adjusted correctly")
    #_(is (= 200M (:quantity (accounts/reload ira)))
          "The account has the correct balance after the transfer.")))

(def ^:private rev-split-context
  (assoc purchase-context :trades [{:type :purchase
                                    :trade-date (t/local-date 2016 3 2)
                                    :entity-id "Personal"
                                    :account-id "IRA"
                                    :commodity-id "AAPL"
                                    :shares 1500M
                                    :value 30000M}]))

(deftest reverse-split-a-commodity
  (let  [ctx (realize rev-split-context)
         account (find-account ctx "IRA")
         commodity (find-commodity ctx "AAPL")
         result (trading/split {:split-date (t/local-date 2016 4 1)
                                :account-id (:id account)
                                :commodity-id (:id commodity)
                                :shares-gained -1350M})
         lots (lots/search {:account-id (:id account)
                            :commodity-id (:id commodity)})]
    (is (= "Split shares of AAPL 1 for 10"
           (get-in result [:transaction :description]))
        "The transaction has the correct description")
    (is (= :credit (get-in result [:transaction :items 0 :action]))
        "The transaction item has the correct action")
    (is (= [{:purchase-date (t/local-date 2016 3 2)
             :shares-owned 150M
             :purchase-price 200M}]
           (map #(select-keys % [:purchase-date
                                 :shares-owned
                                 :purchase-price])
                lots))
        "The lot is updated correctly.")))
