(ns clj-money.trading-test
  (:require [clojure.test :refer [use-fixtures deftest is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.util :as util :refer [model=]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as transactions]
            [clj-money.trading :as trading]))

(use-fixtures :each reset-db)

(defn- items-by-account
  [account-id]
  (map #(dissoc %
                :id
                :transaction-item/polarized-amount
                :transaction-item/entity-id
                :transaction-item/transaction-id
                :transaction-item/reconciled?
                :transaction-item/reconciliation-id
                :transaction-item/created-at
                :transaction-item/updated-at
                :transaction-item/reconciliation-status)
       (transactions/items-by-account
        account-id
        [(t/local-date 2015 1 1)
         (t/local-date 2017 12 31)])))

(defn- item-by-account
  [acc transaction]
  {:pre [acc transaction]}

  (->> (:transaction/items transaction)
         (filter #(model= acc (:transaction-item/account %)))
         first))

(def ^:private purchase-context
  [(factory :user {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "Apple, Inc."
               :entity "Personal"
               :symbol "AAPL"
               :type :stock
               :exchange :nasdaq}
   #:account{:name "IRA"
             :entity "Personal"
             :type :asset}
   #:account{:name "Opening balances"
             :entity "Personal"
             :type :income}
   #:account{:name "Long-term Capital Gains"
             :entity "Personal"
             :type :income}
   #:account{:name "Long-term Capital Losses"
             :entity "Personal"
             :type :expense}
   #:account{:name "Short-term Capital Gains"
             :entity "Personal"
             :type :income}
   #:account{:name "Short-term Capital Losses"
             :entity "Personal"
             :type :expense}
   #:account{:name "Investment Expenses"
             :entity "Personal"
             :type :expense}
   #:account{:name "Checking"
             :entity "Personal"
             :type :asset}
   #:transaction{:transaction-date (t/local-date 2016 1 1)
                 :entity "Personal"
                 :description "Opening balance"
                 :debit-account "IRA"
                 :credit-account "Opening balances"
                 :quantity 2000M}])

(defn- purchase-attributes []
  {:commodity (find-commodity "AAPL")
   :account (find-account "IRA")
   :trade-date (t/local-date 2016 1 2)
   :shares 100M
   :value 1000M})

(deftest purchase-a-commodity
  (with-context purchase-context
    (let [personal (find-entity "Personal")
          ira (find-account "IRA")
          commodity (find-commodity "AAPL")
          result (trading/buy (purchase-attributes))]
      (testing "The transaction"
        (is (comparable? #:transaction{:entity (util/->model-ref personal)
                                       :transaction-date (t/local-date 2016 1 2)
                                       :description "Purchase 100 shares of AAPL at 10.000"
                                       :value 1000M
                                       :lot-items [#:lot-item{:lot-action :buy
                                                              :transaction-date (t/local-date 2016 1 2)
                                                              :shares 100M
                                                              :price 10M}]
                                       :items [#:transaction-item{:action :credit
                                                                  :quantity 1000M
                                                                  :value 1000M
                                                                  :account (util/->model-ref (:account result))}
                                               #:transaction-item{:action :debit
                                                                  :quantity 100M
                                                                  :value 1000M
                                                                  :account (util/->model-ref (:commodity-account result))}]}
                         (:transaction result))
            "A transaction is created and returned"))
      (testing "The commodity account"
        (is (comparable? #:account{:name "AAPL"
                                   :commodity (util/->model-ref commodity)
                                   :entity (util/->model-ref personal)
                                   :type :asset
                                   :parent (util/->model-ref ira)
                                   :system-tags #{:tradable}
                                   :price-as-of (t/local-date 2016 1 2)
                                   :quantity 100M
                                   :value 1000M}
                         (models/find-by #:account{:commodity commodity
                                                   :entity personal}))
            "An account to track shares of the commodity is created"))
      (testing "The trading account"
        (is (comparable? #:account{:name "IRA"
                                   :quantity 1000M
                                   :value 1000M}
                         (models/find ira))
            "The trading account balance is updated to reflect money paid out"))
      (testing "The lot"
        (is (comparable? #:lot{:shares-purchased 100M
                               :shares-owned 100M
                               :purchase-price 10M
                               :purchase-date (t/local-date 2016 1 2)}
                         (:lot result))))
      (testing "The trading account"
        (is (contains? (:account/system-tags (models/find ira))
                       :trading)
            "The :trading tag is added to the trading account"))
      (testing "The price"
        (is (comparable? #:price{:price 10M
                                 :trade-date (t/local-date 2016 1 2)}
                         (:price result))
            "The price is returned")))))

(deftest purchase-a-commodity-with-a-fee
  (with-context purchase-context
    (let [ira (find-account "IRA")
          inv-exp (find-account "Investment Expenses")]
      (-> (purchase-attributes)
          (assoc :fee 5M
                 :fee-account inv-exp)
          trading/buy)
      (is (= 995M (:account/quantity (models/find ira)))
          "The investment account balance reflects the fee")
      (is (= 5M (:account/quantity (models/find inv-exp)))
          "The investment expense account reflects the fee"))))

(defn- assert-invalid-purchase
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (trading/buy attr))))

(deftest purchase-requires-a-trade-date
  (with-context purchase-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :trade-date)
      {:trade-date ["Trade date is required"]})))

(deftest purchase-requires-a-number-of-shares
  (with-context purchase-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :shares)
      {:shares ["Shares is required"]})))

(deftest purchase-requires-a-value
  (with-context purchase-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :value)
      {:value ["Value is required"]})))

; (defn- sale-attributes
;   [context]
;   (let [[ira
;          lt-gains
;          st-gains
;          lt-loss
;          st-loss] (find-accounts context
;                                  "IRA"
;                                  "Long-term Capital Gains"
;                                  "Short-term Capital Gains"
;                                  "Long-term Capital Losses"
;                                  "Short-term Capital Losses")]
;     {:commodity-id (:id (find-commodity context "AAPL"))
;      :account-id (:id ira)
;      :lt-capital-gains-account-id (:id lt-gains)
;      :lt-capital-loss-account-id (:id lt-loss)
;      :st-capital-gains-account-id (:id st-gains)
;      :st-capital-loss-account-id (:id st-loss)
;      :inventory-method :fifo
;      :trade-date (t/local-date 2017 3 2)
;      :shares 25M
;      :value 375M})) ; Sell at $15/share or $125 gain
; 
; (def ^:private sale-context
;   (assoc purchase-context
;          :trades [{:type :purchase
;                    :account-id "IRA"
;                    :commodity-id "AAPL"
;                    :trade-date (t/local-date 2016 3 2)
;                    :shares 100M
;                    :value 1000M}]))
; 
; (deftest sell-a-commodity-for-a-gain
;   (let [context (realize sale-context)
;         entity (find-entity context "Personal")
;         aapl (find-commodity context "AAPL")
;         {:keys [transaction
;                 price
;                 lots]
;          :as result} (trading/sell (sale-attributes context))
;         ira (find-account context "IRA")
;         ltcg (find-account context "Long-term Capital Gains")
;         aapl-acc (accounts/find-by {:entity-id (:id entity)
;                                     :commodity-id (:id aapl)})
;         lot (lots/find-by {:commodity-id (:id aapl)
;                            :account-id (:id ira)
;                            :purchase-date (t/local-date 2016 3 2)})]
;     (is price "The result contains a price")
;     (is (valid? price) "The price is valid")
;     (is lots "The result contains the lots affected")
;     (doseq [lot lots]
;       (is (valid? lot) "Each lot is valid"))
;     (is (= 75M (:shares-owned lot))
;         "The shares-owned value of the original lot is updated")
;     (is transaction "The result contains the transaction record")
;     (is (valid? transaction) "The transaction is valid")
;     (is (comparable? {:name "AAPL"
;                       :value 1125M
;                       :price-as-of (t/local-date 2017 3 2)}
;                      (accounts/reload aapl-acc))
;         "The commodity account is updated wth new value and price date")
;     (is (comparable? {:action :debit
;                       :value 375M
;                       :quantity 375M}
;                      (item-by-account ira transaction))
;         "The trading account is debited the total proceeds from the purchase")
;     (is (comparable? {:action :credit
;                       :value 125M
;                       :quantity 125M}
;                      (item-by-account ltcg transaction))
;         "The capital gains account is credited the amount received above the original cost of the shares.")
;     (is (comparable? {:action :credit
;                       :value 250M
;                       :quantity 25M}
;                      (item-by-account aapl-acc transaction))
;         "The commodity account is credited the number of shares and purchase value of the shares.")
;     (testing "entity settings"
;       (let [expected (select-keys result [:lt-capital-gains-account-id
;                                           :st-capital-gains-account-id
;                                           :lt-capital-loss-account-id
;                                           :st-capital-loss-account-id
;                                           :inventory-method])]
;         (is (comparable? expected (:settings (entities/find entity)))
;             "The entity settings are updated with default account ids")))))
; 
; (deftest sell-a-commodity-for-a-loss
;   (let [context (realize sale-context)
;         entity (find-entity context "Personal")
;         aapl (find-commodity context "AAPL")
;         attr (assoc (sale-attributes context) :value 200M) ; 25 shares, $50 loss
;         {:keys [lots transaction price]} (trading/sell attr)
;         ira (find-account context "IRA")
;         ltcl (find-account context "Long-term Capital Losses")
;         aapl-acc (accounts/find-by {:entity-id (:id entity)
;                                     :commodity-id (:id aapl)})
;         lot (lots/find-by {:commodity-id (:id aapl)
;                            :account-id (:id ira)
;                            :purchase-date (t/local-date 2016 3 2)})]
;     (is (= 8M (:price price))
;         "The result contains the correct price")
;     (is (valid? price) "The price is valid")
;     (is lots "The result contains the lots affected")
;     (doseq [lot lots]
;       (is (valid? lot) "Each lot is valid"))
;     (is (= 75M (:shares-owned lot))
;         "The shares-owned value of the original lot is updated")
;     (is transaction "The result contains the transaction record")
;     (is (valid? transaction) "The transaction is valid")
;     (is (comparable? {:action :debit
;                       :value 200M
;                       :quantity 200M}
;                      (item-by-account ira transaction))
;         "The trading account is debited the total proceeds from the purchase")
;     (is (comparable? {:action :debit
;                       :value 50M
;                       :quantity 50M}
;                      (item-by-account ltcl transaction))
;         "The capital loss account is debited the cost the shares less the sale proceeds")
;     (is (comparable? {:action :credit
;                       :value 250M
;                       :quantity 25M}
;                      (item-by-account aapl-acc transaction))
;         "The commodity account is credited the number of shares and purchase value of the shares.")))
; 
; (def ^:private auto-create-context
;   (update-in sale-context [:accounts] (fn [accounts]
;                                         (remove #(re-find #"Capital" (:name %)) accounts))))
; 
; (deftest auto-create-gains-accounts
;   (let [context (realize auto-create-context)
;         _ (trading/sell (sale-attributes context))
;         entity (entities/find (find-entity context "Personal"))
;         ltcg (accounts/find-by {:entity-id (:id entity)
;                                 :name "Long-term Capital Gains"})
;         stcg (accounts/find-by {:entity-id (:id entity)
;                                 :name "Short-term Capital Gains"})
;         ltcl (accounts/find-by {:entity-id (:id entity)
;                                 :name "Long-term Capital Losses"})
;         stcl (accounts/find-by {:entity-id (:id entity)
;                                 :name "Short-term Capital Losses"})]
;     (is (comparable? {:type :income}
;                      ltcg)
;         "The long-term capital gains account is an income account")
;     (is (= (get-in entity [:settings :lt-capital-gains-account-id])
;            (:id ltcg))
;         "The Long-term Capital Gains account id is placed in the entity settings")
;     (is (comparable? {:type :income}
;                      stcg)
;         "The short-term capital gains account is an income account")
;     (is (= (get-in entity [:settings :st-capital-gains-account-id])
;            (:id stcg))
;         "The Short-term Capital Gains account id is placed in the entity settings")
;     (is (comparable? {:type :expense}
;                      ltcl)
;         "The long-term capital losses account is an expense account")
;     (is (= (get-in entity [:settings :lt-capital-loss-account-id])
;            (:id ltcl))
;         "The Long-term Capital Losses account id is placed in the entity settings")
;     (is (comparable? {:type :expense}
;                      stcl)
;         "The short-term capital losses account is an expense account")
;     (is (= (get-in entity [:settings :st-capital-loss-account-id])
;            (:id stcl))
;         "The Short-term Capital Losses account id is placed in the entity settings")))
; 
; (deftest sell-a-commodity-with-a-fee
;   (let [context (realize sale-context)
;         ira (find-account context "IRA")
;         inv-exp (->> context
;                      :accounts
;                      (filter #(= "Investment Expenses" (:name %)))
;                      first)]
;     (trading/sell (-> context
;                       sale-attributes
;                       (assoc :fee 5M
;                              :fee-account-id (:id inv-exp))))
;     (is (= 1370M (:quantity (accounts/reload ira)))
;         "The investment account balance reflects the fee")
;     (is (= 5M (:quantity (accounts/reload inv-exp)))
;         "The investment fee account balance reflects the fee")))
; 
; (deftest sales-requires-a-trade-date
;   (let [context (realize sale-context)
;         result (trading/sell (-> context
;                                  sale-attributes
;                                  (dissoc :trade-date)))]
;     (is (invalid? result [:trade-date] "Trade date is required"))))
; 
; (deftest sales-requires-a-number-of-shares
;   (let [context (realize sale-context)
;         result (trading/sell (-> context
;                                  sale-attributes
;                                  (dissoc :shares)))]
;     (is (invalid? result [:shares] "Shares is required"))))
; 
; (deftest sales-requires-a-value
;   (let [context (realize sale-context)
;         result (trading/sell (-> context
;                                  sale-attributes
;                                  (dissoc :value)))]
;     (is (invalid? result [:value] "Value is required"))))
; 
; (deftest selling-a-commodity-for-a-profit-increases-the-balance-of-the-account
;   (let [context (realize purchase-context)
;         [ira] (:accounts context)
;         commodity (find-commodity context "AAPL")
;         _ (trading/buy {:commodity-id (:id commodity)
;                         :account-id (:id ira)
;                         :trade-date (t/local-date 2016 1 2)
;                         :shares 100M
;                         :value 1000M})
;         _ (trading/sell (-> context
;                             sale-attributes
;                             (assoc :shares 50M :value 560M)))
;         new-balance (:quantity (accounts/reload ira))]
;     (is (= 1560M new-balance) "The account balance decreases by the amount of the purchase")))
; 
; (deftest selling-a-commodity-updates-a-lot-record
;   (let [context (realize sale-context)
;         ira (find-account context "IRA")
;         commodity (find-commodity context "AAPL")
;         _ (trading/sell (-> context
;                             sale-attributes
;                             (assoc :shares 25M :value 375M)))
;         lots (map #(dissoc % :id :created-at :updated-at)
;                   (lots/search {:account-id (:id ira)
;                                 :commodity-id (:id commodity)}))
;         expected [{:purchase-date (t/local-date 2016 3 2)
;                    :account-id (:id ira)
;                    :commodity-id (:id commodity)
;                    :shares-purchased 100M
;                    :shares-owned 75M
;                    :purchase-price 10M}]]
;     (is (= expected lots) "The lot is updated to reflect the sale")))
; 
; (deftest selling-a-commodity-for-a-profit-after-1-year-credits-long-term-capital-gains
;   (let [context (realize sale-context)
;         lt-capital-gains (find-account context "Long-term Capital Gains")
;         _ (trading/sell (-> context
;                             sale-attributes
;                             (assoc :shares 25M :value 375M)))
;         gains-items (items-by-account (:id lt-capital-gains))
;         expected [{:transaction-date (t/local-date 2017 3 2)
;                    :description "Sell 25 shares of AAPL at 15.000"
;                    :action :credit
;                    :account-id (:id lt-capital-gains)
;                    :quantity 125M
;                    :memo "Sell 25 shares of AAPL at 15.000"}]]
;     (is (seq-of-maps-like? expected gains-items) "The capital gains account is credited the correct amount")))
; 
; (deftest selling-a-commodity-for-a-profit-before-1-year-credits-short-term-capital-gains
;   (let [context (realize sale-context)
;         st-capital-gains (find-account context "Short-term Capital Gains")
;         _ (trading/sell (-> context
;                             sale-attributes
;                             (assoc :shares 25M
;                                    :value 375M
;                                    :trade-date (t/local-date 2017 3 1))))
;         gains-items (items-by-account (:id st-capital-gains))
;         expected [{:transaction-date (t/local-date 2017 3 1)
;                    :description "Sell 25 shares of AAPL at 15.000"
;                    :action :credit
;                    :account-id (:id st-capital-gains)
;                    :quantity 125M
;                    :memo "Sell 25 shares of AAPL at 15.000"}]]
;     (is (seq-of-maps-like? expected gains-items) "The capital gains account is credited the correct amount")))
; 
; ; Selling a commodity updates a lot record (FILO updates the most recent, FIFO updates the oldest)
; 
; (defn- map-accounts
;   [context & account-names]
;   (map (fn [account-name]
;          (->> context
;               :accounts
;               (filter #(= account-name (:name %)))
;               first))
;        account-names))
; 
; (deftest lifo-sale
;   (let [context (realize purchase-context)
;         commodity (find-commodity context "AAPL")
;         [ira
;          lt-gains
;          st-gains
;          lt-loss
;          st-loss] (map-accounts context
;                                 "IRA"
;                                 "Long-term Capital Gains"
;                                 "Short-term Capital Gains"
;                                 "Long-term Capital Losses"
;                                 "Short-term Capital Losses")
;         _ (trading/buy {:trade-date (t/local-date 2015 3 2)
;                         :account-id (:id ira)
;                         :commodity-id (:id commodity)
;                         :shares 100M
;                         :value 1000M})
;         _ (trading/buy {:trade-date (t/local-date 2016 3 2)
;                         :account-id (:id ira)
;                         :commodity-id (:id commodity)
;                         :shares 100M
;                         :value 2000M})
;         _ (trading/sell {:trade-date (t/local-date 2017 3 2)
;                          :account-id (:id ira)
;                          :commodity-id (:id commodity)
;                          :shares 50M
;                          :value 1500M
;                          :inventory-method :lifo
;                          :lt-capital-gains-account-id (:id lt-gains)
;                          :st-capital-gains-account-id (:id st-gains)
;                          :lt-capital-loss-account-id (:id lt-loss)
;                          :st-capital-loss-account-id (:id st-loss)})
;         actual (->> (lots/search {:commodity-id (:id commodity)
;                                   :account-id (:id ira)})
;                     (sort-by :purchase-date)
;                     (map #(dissoc %
;                                   :id
;                                   :created-at
;                                   :updated-at
;                                   :commodity-id
;                                   :account-id)))
;         expected [{:purchase-date (t/local-date 2015 3 2)
;                    :shares-purchased 100M
;                    :shares-owned 100M
;                    :purchase-price 10M}
;                   {:purchase-date (t/local-date 2016 3 2)
;                    :shares-purchased 100M
;                    :shares-owned 50M
;                    :purchase-price 20M}]]
;     (is (= expected actual) "Shares are sold from the most recent lot")))
; 
; (deftest fifo-sale
;   (let [context (realize
;                  (update-in purchase-context
;                             [:entities 0]
;                             #(assoc-in % [:settings :inventory-method] :fifo)))
;         commodity (find-commodity context "AAPL")
;         [ira
;          lt-gains
;          st-gains
;          lt-loss
;          st-loss] (map-accounts context
;                                 "IRA"
;                                 "Long-term Capital Gains"
;                                 "Short-term Capital Gains"
;                                 "Long-term Capital Gains"
;                                 "Short-term Capital Gains")
;         _ (trading/buy {:trade-date (t/local-date 2015 3 2)
;                         :account-id (:id ira)
;                         :commodity-id (:id commodity)
;                         :shares 100M
;                         :value 1000M})
;         _ (trading/buy {:trade-date (t/local-date 2016 3 2)
;                         :account-id (:id ira)
;                         :commodity-id (:id commodity)
;                         :shares 100M
;                         :value 2000M})
;         _ (trading/sell {:trade-date (t/local-date 2017 3 2)
;                          :account-id (:id ira)
;                          :commodity-id (:id commodity)
;                          :shares 50M
;                          :value 1500M
;                          :lt-capital-gains-account-id (:id lt-gains)
;                          :st-capital-gains-account-id (:id st-gains)
;                          :lt-capital-loss-account-id (:id lt-loss)
;                          :st-capital-loss-account-id (:id st-loss)})
;         actual (->> (lots/search {:commodity-id (:id commodity)
;                                   :account-id (:id ira)})
;                     (sort-by :purchase-date)
;                     (map #(dissoc %
;                                   :id
;                                   :created-at
;                                   :updated-at
;                                   :commodity-id
;                                   :account-id)))
;         expected [{:purchase-date (t/local-date 2015 3 2)
;                    :shares-purchased 100M
;                    :shares-owned 50M
;                    :purchase-price 10M}
;                   {:purchase-date (t/local-date 2016 3 2)
;                    :shares-purchased 100M
;                    :shares-owned 100M
;                    :purchase-price 20M}]]
;     (is (= expected actual) "Shares are sold from the most recent lot")))
; 
; (deftest undo-a-purchase
;   (let [context (realize purchase-context)
;         ira (find-account context "IRA")
;         commodity (find-commodity context "AAPL")
;         purchase (trading/buy {:trade-date (t/local-date 2017 3 2)
;                                :shares 100M
;                                :commodity-id (:id commodity)
;                                :account-id (:id ira)
;                                :value 1000M})]
;     (trading/unbuy (:transaction purchase))
;     ; TODO Should we delete the price that was created?
;     (testing "the account balance"
;       (is (= 2000M (:quantity (accounts/reload ira)))
;           "The account balance is restored"))
;     (testing "the affected lots"
;       (is (= [] (lots/search {:account-id (:id ira)}))
;           "The lot is deleted"))))
; 
; (deftest cannot-undo-a-purchase-if-shares-have-been-sold
;   (let [context (realize purchase-context)
;         ira (find-account context "IRA")
;         commodity (find-commodity context "AAPL")
;         purchase (trading/buy {:trade-date (t/local-date 2017 3 2)
;                                :shares 100M
;                                :commodity-id (:id commodity)
;                                :account-id (:id ira)
;                                :value 1000M})
;         _ (trading/sell (sale-attributes context))]
;     (is (thrown-with-msg? IllegalStateException #"Cannot undo"
;                           (trading/unbuy (:transaction purchase))))))
; 
; (deftest undo-a-sale
;   (let [context (realize purchase-context)
;         ira (find-account context "IRA")
;         commodity (find-commodity context "AAPL")
;         _ (trading/buy {:trade-date (t/local-date 2017 3 2)
;                         :shares 100M
;                         :commodity-id (:id commodity)
;                         :account-id (:id ira)
;                         :value 1000M})
;         sale (trading/sell (sale-attributes context))
;         _ (trading/unsell (:transaction sale))]
;     ; IRA balance balance before purchase: $2,000
;     ;                      after purchase: $1,000
;     ;                          after sale: $1,375
;     ;                        after unsale: $1,000
;     (is (= 1000M (:quantity (accounts/reload ira)))
;         "The account balance is restored")
;     (testing "the affected lots"
;       (doseq [lot (map lots/find (:lots sale))]
;         (is (= (:shares-owned lot) (:shares-purchased lot))
;             "The shares-owned should be restored")))))
; 
; (def ^:private transfer-context
;   (update-in sale-context [:accounts] conj {:name "IRA 2"
;                                             :type :asset}))
; 
; (deftest transfer-a-commodity
;   (let [context (realize transfer-context)
;         [ira ira-2] (find-accounts context "IRA" "IRA 2")
;         commodity (find-commodity context "AAPL")
;         result (trading/transfer {:commodity-id (:id commodity)
;                                   :from-account-id (:id ira)
;                                   :to-account-id (:id ira-2)
;                                   :shares 100M
;                                   :transfer-date (t/local-date 2016 4 2)})
;         [ira-commodity-account
;          ira-2-commodity-account] (->> [ira ira-2]
;                                        (map :id)
;                                        (map #(accounts/search
;                                               {:parent-id %
;                                                :commodity-id (:id commodity)}))
;                                        (map first))
;         actual-lots (map #(dissoc % :created-at :updated-at :id)
;                          (lots/search {:commodity-id (:id commodity)}))
;         expected-lots [{:commodity-id (:id commodity)
;                         :account-id (:id ira-2)
;                         :shares-owned 100M
;                         :purchase-price 10M
;                         :shares-purchased 100M
;                         :purchase-date (t/local-date 2016 3 2)}]
;         expected-transaction {:transaction-date (t/local-date 2016 4 2)
;                               :description "Transfer 100 shares of AAPL"
;                               :entity-id (:entity-id commodity)
;                               :items [{:action :credit
;                                        :quantity 100M
;                                        :value 1000M
;                                        :balance 0M
;                                        :account-id (:id ira-commodity-account)}
;                                       {:action :debit
;                                        :quantity 100M
;                                        :value 1000M
;                                        :balance 100M
;                                        :account-id (:id ira-2-commodity-account)}]}
;         actual-transaction (-> (:transaction result)
;                                (select-keys [:entity-id
;                                              :transaction-date
;                                              :description
;                                              :items])
;                                (update-in [:items]
;                                           (fn [items]
;                                             (map #(select-keys
;                                                    %
;                                                    [:action
;                                                     :account-id
;                                                     :quantity
;                                                     :action
;                                                     :value
;                                                     :balance])
;                                                  items))))]
;     (is result "A non-nil result is returned")
;     (is (valid? result) "The result is valid")
;     (is (= expected-transaction actual-transaction)
;         "The correct transaction is returned")
;     (is (= expected-lots actual-lots)
;         "The lots are adjusted correctly.")
;     ; Original account balance was 2,000, we bought 1,000 worth of
;     ; shares of AAPL, then transfered those shares out of the account
;     ; leaving 1,000 in cash
;     (is (= 1000M (:quantity (accounts/reload ira)))
;         "The balance in the 'from' account is updated correctly")
;     ; No money was ever addedto the second account, so the balance
;     ; is still 0
;     (is (= 0M (:quantity (accounts/reload ira-2)))
;         "The balance in the 'to' account is updated correclty")))
; 
; (deftest split-a-commodity
;   (let [context (realize sale-context)
;         ira (find-account context "IRA")
;         commodity (find-commodity context "AAPL")
;         commodity-account (accounts/find-by {:commodity-id (:id commodity)
;                                              :entity-id (:entity-id commodity)})
;         result (trading/split {:commodity-id (:id commodity)
;                                :account-id (:id ira)
;                                :shares-gained 100M
;                                :split-date (t/local-date 2016 3 3)})
;         lots (lots/search {:commodity-id (:id commodity)})
;         actual-lots (map #(dissoc % :id :created-at :updated-at :commodity-id) lots)
;         expected-lots [{:purchase-date (t/local-date 2016 3 2)
;                         :account-id (:id ira)
;                         :purchase-price 5M
;                         :shares-purchased 200M
;                         :shares-owned 200M}]
;         expected-transaction {:entity-id (:entity-id commodity)
;                               :transaction-date (t/local-date 2016 3 3)
;                               :description "Split shares of AAPL 2 for 1"
;                               :value 0M
;                               :items [{:action :debit
;                                        :account-id (:id commodity-account)
;                                        :quantity 100M
;                                        :polarized-quantity 100M
;                                        :balance 200M
;                                        :value 0M
;                                        :description "Split shares of AAPL 2 for 1"}]}]
;     (is (valid? result) "The result has no validation errors")
;     (is (= 2M (:ratio result))
;         "The correct split ratio is returned")
;     (is (comparable? expected-transaction (:transaction result))
;         "The result contains the transaction that was created")
;     (is (= expected-lots actual-lots)
;         "The lots are adjusted correctly")
;     (is (= 1000M (:quantity (accounts/reload ira)))
;           "The account has the correct balance after the transfer.")))
; 
; (def ^:private rev-split-context
;   (assoc purchase-context :trades [{:type :purchase
;                                     :trade-date (t/local-date 2016 3 2)
;                                     :entity-id "Personal"
;                                     :account-id "IRA"
;                                     :commodity-id "AAPL"
;                                     :shares 1500M
;                                     :value 30000M}]))
; 
; (deftest reverse-split-a-commodity
;   (let  [ctx (realize rev-split-context)
;          account (find-account ctx "IRA")
;          commodity (find-commodity ctx "AAPL")
;          result (trading/split {:split-date (t/local-date 2016 4 1)
;                                 :account-id (:id account)
;                                 :commodity-id (:id commodity)
;                                 :shares-gained -1350M})
;          lots (lots/search {:account-id (:id account)
;                             :commodity-id (:id commodity)})]
;     (is (= "Split shares of AAPL 1 for 10"
;            (get-in result [:transaction :description]))
;         "The transaction has the correct description")
;     (is (= :credit (get-in result [:transaction :items 0 :action]))
;         "The transaction item has the correct action")
;     (is (= [{:purchase-date (t/local-date 2016 3 2)
;              :shares-owned 150M
;              :purchase-price 200M}]
;            (map #(select-keys % [:purchase-date
;                                  :shares-owned
;                                  :purchase-price])
;                 lots))
;         "The lot is updated correctly.")))
; 
; #_(def delete-trading-transaction-context
;   (conj base-context
;         #:account{:name "IRA"
;                   :type :asset
;                   :entity "Personal"}
;         #:commodity{:name "Apple, Inc."
;                     :symbol "AAPL"
;                     :exchange :nasdaq
;                     :type :stock
;                     :entity "Personal"}
;         #:trade{:type :buy
;                 :commodity "AAPL"
;                 :account "IRA"
;                 :shares 100M
;                 :value 1000M
;                 :trade-date (t/local-date 2015 1 1)}))
; 
; #_(deftest deleting-trading-transactions-deletes-lots-created-by-the-transaction
;   (with-context delete-trading-transaction-context
;     (let [[{:keys [transaction lot]}] (:trades *context*)]
;       (is lot "The lot is present before deleting the transaction")
;       (transactions/delete transaction)
;       (is (nil? (lots/find lot)) "The lot is not retreivable after deleting the transaction."))))
; 
; #_(def ^:private trading-update-context
;   (conj basic-context
;         #:commodity{:name "Apple, Inc."
;                     :entity "Personal"
;                     :symbol "AAPL"
;                     :type :stock
;                     :exchange :nasdaq}
;         #:account{:name "IRA"
;                   :entity "Personal"
;                   :type :asset}
;         #:trade{:trade-date (t/local-date 2015 1 1)
;                 :type :buy
;                 :commodity "AAPL"
;                 :account "IRA"
;                 :shares 100M
;                 :value 1000M}))
; 
; #_(deftest update-a-trading-transaction
;   (with-context trading-update-context
;     ; TODO: are there parts that can be changed?
;     (testing "the date and quantiies cannot be updated"
;       (let [result (-> (get-in *context* [:trades 0 :transaction])
;                                 (assoc :transaction-date (t/local-date 2015 2 1))
;                                 transactions/update)]
;         (is (= ["A trading transaction cannot be updated."]
;                (v/flat-error-messages result)))))))
