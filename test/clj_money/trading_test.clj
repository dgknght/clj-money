(ns clj-money.trading-test
  (:require [clojure.test :refer [use-fixtures deftest is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account
                                            find-commodity
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.trading :as trading]))

(use-fixtures :each reset-db)

(def ^:private base-context
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
   #:account{:name "Investment Expenses"
             :entity "Personal"
             :type :expense}
   #:account{:name "Checking"
             :entity "Personal"
             :type :asset}
   #:account {:name "Dividends"
              :entity "Personal"
              :type :income}
   #:transaction{:transaction-date (t/local-date 2016 1 1)
                 :entity "Personal"
                 :description "Opening balance"
                 :debit-account "IRA"
                 :credit-account "Opening balances"
                 :quantity 2000M}])

(defn- purchase-attributes []
  #:trade{:commodity (find-commodity "AAPL")
          :account (find-account "IRA")
          :date (t/local-date 2016 1 2)
          :shares 100M
          :value 1000M})

(deftest purchase-a-commodity
  (with-context base-context
    (let [personal (find-entity "Personal")
          ira (find-account "IRA")
          commodity (find-commodity "AAPL")
          result (trading/buy (purchase-attributes))]
      (testing "The transaction"
        (is (seq-of-maps-like?
              [#:transaction{:entity (util/->entity-ref personal)
                             :transaction-date (t/local-date 2016 1 2)
                             :description "Purchase 100.000 shares of AAPL at 10.000"
                             :value 1000M
                             :lot-items
                             [#:lot-item{:action :buy
                                         :shares 100M
                                         :price 10M}]

                             :items
                             [#:transaction-item{:action :credit
                                                 :account (util/->entity-ref ira)
                                                 :quantity 1000M}
                              #:transaction-item{:action :debit
                                                 :account (util/->entity-ref (:trade/commodity-account result))
                                                 :quantity 100M
                                                 :value 1000M}]}]

              (:trade/transactions result))
            "A transaction is created and returned"))
      (testing "The commodity account"
        (is (comparable? #:account{:name "AAPL"
                                   :commodity (util/->entity-ref commodity)
                                   :entity (util/->entity-ref personal)
                                   :type :asset
                                   :parent (util/->entity-ref ira)
                                   :system-tags #{:tradable}
                                   :price-as-of (t/local-date 2016 1 2)}
                         (entities/find-by #:account{:commodity commodity
                                                     :entity personal}))
            "An account to track shares of the commodity is created"))
      (testing "The lot"
        (is (comparable? #:lot{:shares-purchased 100M
                               :shares-owned 100M
                               :purchase-price 10M
                               :purchase-date (t/local-date 2016 1 2)}
                         (:trade/lot result))))
      (testing "The trading account"
        (is (contains? (:account/system-tags (entities/find ira))
                       :trading)
            "The :trading tag is added to the trading account"))
      (testing "The price"
        (is (comparable? #:price{:value 10M
                                 :trade-date (t/local-date 2016 1 2)}
                         (:trade/price result))
            "The price is returned")))))

(deftest ^:multi-threaded propagate-a-purchase
  (with-context base-context
    (let [personal (find-entity "Personal")
          ira (find-account "IRA")
          commodity (find-commodity "AAPL")]
      (trading/buy-and-propagate (purchase-attributes))
      (testing "The commodity account"
        (is (comparable? #:account{:name "AAPL"
                                   :quantity 100M
                                   :value 1000M}
                         (entities/find-by #:account{:commodity commodity
                                                     :entity personal}))
            "An account to track shares of the commodity is has propagated values"))
      (testing "The trading account"
        (is (comparable? #:account{:name "IRA"
                                   :quantity 1000M
                                   :value 1000M}
                         (entities/find ira))
            "The trading account balance is updated to reflect money paid out")))))

(deftest ^:multi-threaded purchase-a-commodity-with-a-fee
  (with-context base-context
    (let [ira (find-account "IRA")
          inv-exp (find-account "Investment Expenses")]
      (-> (purchase-attributes)
          (assoc :trade/fee 5M
                 :trade/fee-account inv-exp)
          (trading/buy-and-propagate))
      (is (= 995M (:account/quantity (entities/find ira)))
          "The investment account balance reflects the fee")
      (is (= 5M (:account/quantity (entities/find inv-exp)))
          "The investment expense account reflects the fee"))))

(deftest ^:multi-threaded reinvest-a-dividend
  (with-context base-context
    (let [dividends (find-account "Dividends")
          ira (entities/find (find-account "IRA"))
          [{:trade/keys [transactions]}] (-> #:trade{:commodity (find-commodity "AAPL")
                                                     :account (find-account "IRA")
                                                     :date (t/local-date 2016 2 2)
                                                     :shares 4.5M
                                                     :value 50M}
                                             (assoc :trade/dividend? true
                                                    :trade/dividend-account dividends)
                                             (trading/buy-and-propagate))]
      (is (comparable? #:transaction{:description "Dividend received from AAPL"}
                       (first transactions))
          "The transaction for the receipt of the dividend is returned")
      (is (comparable? #:transaction{:description "Reinvest dividend of 50.00: purchase 4.500 shares of AAPL at 11.110"}
                       (second transactions))
          "The transaction for the purchase of shares with dividend is returned")
      (is (comparable? #:account{:quantity 50M
                                 :value 50M}
                       (entities/find dividends))
          "The dividend account is debited for the amount of the purchase")
      (is (= (:account/quantity ira)
             (:account/quantity (entities/find ira)))
          "The trading account balance is unchanged."))))

(defn- assert-invalid-purchase
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (trading/buy attr))))

(deftest purchase-requires-a-trade-date
  (with-context base-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :trade/date)
      {:trade/date ["Date is required"]})))

(deftest purchase-requires-a-number-of-shares
  (with-context base-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :trade/shares)
      {:trade/shares ["Shares is required"]})))

(deftest purchase-requires-a-value
  (with-context base-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :trade/value)
      {:trade/value ["Value is required"]})))

; Sell 25 shares at $15/share and $125 gain
; value before sale: 1,500
; value after sale:  1,125
;
; The trading account:
; quantity before: 1,000
; quantity after: 1,375
(defn- sale-attributes []
  #:trade{:commodity (find-commodity "AAPL")
          :account (find-account "IRA")
          :inventory-method :fifo
          :date (t/local-date 2017 3 2)
          :shares 25M
          :value 375M})

(def ^:private sale-context
  (conj base-context
        #:trade{:type :purchase
                :entity "Personal"
                :account "IRA"
                :commodity "AAPL"
                :date (t/local-date 2016 3 2)
                :shares 100M
                :value 1000M}))

(deftest sell-a-commodity-for-a-gain
  (with-context sale-context
    (let [result (trading/sell (sale-attributes))
          aapl-acc (entities/find-by #:account{:entity (find-entity "Personal")
                                               :commodity (find-commodity "AAPL")})]
      (testing "The price"
        (is (comparable? #:price{:value 15M
                                 :trade-date (t/local-date 2017 3 2)}
                         (:trade/price result))
            "The price is created and returned"))
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:shares-purchased 100M
                                      :shares-owned 75M}]
                               (:trade/updated-lots result))
            "The affected lots are returned"))
      (testing "The transaction"
        (is (seq-of-maps-like?
              [#:transaction{:transaction-date (t/local-date 2017 3 2)
                             :description "Sell 25.000 shares of AAPL at 15.000 for 125.000 long-term gain"}]
              (:trade/transactions result))
            "The transaction is created and returned")
        (is (seq-of-maps-like?
              [{:transaction-item/action :debit
                :transaction-item/quantity 2000M}
               {:transaction-item/action :credit
                :transaction-item/quantity 1000M}
               {:transaction-item/action :debit
                :transaction-item/quantity 375M}]
              (entities/select
                {:transaction-item/account (find-account "IRA")}))
            "The trading account is debited the total proceeds from the purchase")
        (is (seq-of-maps-like?
              [{:transaction-item/action :debit
                :transaction-item/quantity 100M}
               {:transaction-item/action :credit
                :transaction-item/quantity 25M}]
              (entities/select
                {:transaction-item/account aapl-acc}))
            "The commodity account is credited the number of shares and purchase value of the shares.")))))

(deftest ^:multi-threaded propagate-a-sale
  (with-context sale-context
    (trading/sell-and-propagate (sale-attributes))
    (testing "The commodity account"
      (is (comparable? #:account{:name "AAPL"
                                 :quantity 75M
                                 :value 1125M
                                 :price-as-of (t/local-date 2017 3 2)}
                       (entities/find-by {:account/name "AAPL"}))
          "The commodity account is updated wth new value and price date"))
    (testing "The trading account"
      (is (comparable? #:account{:name "IRA"
                                 :quantity 1375M
                                 :value 1375M}
                       (entities/find-by {:account/name "IRA"}))
          "The trading account is updated wth new value"))))

; sell 25 shares at $8.00 per share and $50 loss
; value before sale: $800.00
; value after sale: $600.00
(deftest sell-a-commodity-for-a-loss
  (with-context sale-context
    (let [result (-> (sale-attributes)
                     (assoc :trade/value 200M)
                     trading/sell)]
      (testing "The price"
        (is (comparable? #:price{:value 8M
                                 :trade-date (t/local-date 2017 3 2)}
                         (:trade/price result))
            "The result contains the new price"))
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:shares-owned 75M}]
                               (:trade/updated-lots result))
            "The result contains the updated lots"))

      (testing "The transaction"
        (is (seq-of-maps-like?
              [#:transaction{:transaction-date (t/local-date 2017 3 2)
                             :description "Sell 25.000 shares of AAPL at 8.000 for 50.000 long-term loss"}]
              (:trade/transactions result))
            "The result contains the transaction")
        (is (seq-of-maps-like?
              [#:transaction-item{:action :debit
                                  :quantity 2000M}  ; fund the account
               #:transaction-item{:action :credit
                                  :quantity 1000M} ; purchase the commodity
               #:transaction-item{:action :debit
                                  :quantity 200M}]  ; sell a portion
              (entities/select
                {:transaction-item/account (find-account "IRA")}))
            "The trading account is debited the total proceeds from the purchase")
        (is (seq-of-maps-like?
              [#:transaction-item{:action :debit
                                  :quantity 100M}
               #:transaction-item{:action :credit
                                  :quantity 25M}]
              (entities/select
                {:transaction-item/account (entities/find-by
                                             #:account{:entity (find-entity "Personal")
                                                       :commodity (find-commodity "AAPL")})}))
            "The commodity account is credited the number of shares and purchase value of the shares.")))))

(deftest ^:multi-threaded sell-a-commodity-with-a-fee
  (with-context sale-context
    (trading/sell-and-propagate
      (assoc (sale-attributes)
             :trade/fee 5M
             :trade/fee-account (find-account "Investment Expenses")))
    ; Opening balance             $2,000
    ; Purchase AAPL     -1,000 -> $1,000
    ; Sell AAPL          + 375 -> $1,375
    ; less the fee        -  5 -> $1,370
    (is (comparable? {:account/quantity 1370M}
                     (entities/find (find-account "IRA")))
        "The investment account balance reflects the fee")
    (is (comparable? {:account/quantity 5M}
                     (entities/find (find-account "Investment Expenses")))
        "The investment fee account balance reflects the fee")))

(defn- assert-invalid-sale
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (trading/sell attr))))

(deftest sale-requires-a-trade-date
  (with-context sale-context
    (assert-invalid-sale
      (dissoc (sale-attributes) :trade/date)
      {:trade/date ["Date is required"]})))

(deftest sale-requires-a-number-of-shares
  (with-context sale-context
    (assert-invalid-sale
      (dissoc (sale-attributes) :trade/shares)
      {:trade/shares ["Shares is required"]})))

(deftest sale-requires-a-value
  (with-context sale-context
    (assert-invalid-sale
      (dissoc (sale-attributes) :trade/value)
      {:trade/value ["Value is required"]})))
 
; Selling a commodity updates a lot record
; (FILO updates the most recent, FIFO updates the oldest)

(def ^:private multi-lot-context
  (conj base-context
        #:trade{:type :purchase
                :entity "Personal"
                :date (t/local-date 2015 3 2)
                :account "IRA"
                :commodity "AAPL"
                :shares 100M
                :value 1000M}
        #:trade{:type :purchase
                :entity "Personal"
                :date (t/local-date 2016 3 2)
                :account "IRA"
                :commodity "AAPL"
                :shares 100M
                :value 2000M}))

(defn- multi-lot-sale-attributes
  [account commodity]
  #:trade{:date (t/local-date 2017 3 2)
          :account account
          :commodity commodity
          :shares 50M
          :value 1500M})

(deftest lifo-sale
  (with-context multi-lot-context
    (let [commodity (find-commodity "AAPL")
          ira (find-account "IRA")]
      (-> (multi-lot-sale-attributes ira commodity)
          (assoc :trade/inventory-method :lifo)
          trading/sell)
      (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2015 3 2)
                                    :shares-purchased 100M
                                    :shares-owned 100M
                                    :purchase-price 10M}
                              #:lot{:purchase-date (t/local-date 2016 3 2)
                                    :shares-purchased 100M
                                    :shares-owned 50M
                                    :purchase-price 20M}]
                             (entities/select #:lot{:commodity commodity
                                                  :account ira}
                                            {:sort [[:lot/purchase-date :asc]]}))
          "Shares are sold from the most recent lot"))))

(deftest fifo-sale
  (with-context multi-lot-context
    (let [commodity (find-commodity "AAPL")
          ira (find-account "IRA")]
      (-> (multi-lot-sale-attributes ira commodity)
          (assoc :trade/inventory-method :fifo)
          trading/sell)
      (is (seq-of-maps-like?
            [#:lot{:purchase-date (t/local-date 2015 3 2)
                   :shares-purchased 100M
                   :shares-owned 50M
                   :purchase-price 10M}
             #:lot{:purchase-date (t/local-date 2016 3 2)
                   :shares-purchased 100M
                   :shares-owned 100M
                   :purchase-price 20M}]
            (entities/select #:lot{:commodity commodity
                                   :account ira}
                             {:sort [[:lot/purchase-date :asc]]}))
          "Shares are sold from the earliest lot"))))

(deftest ^:multi-threaded undo-a-purchase
  (with-context sale-context
    (trading/unbuy-and-propagate
      (find-transaction [(t/local-date 2016 3 2)
                         "Purchase 100.000 shares of AAPL at 10.000"] ))
    ; TODO Should we delete the price that was created?
    (testing "The trading account"
      (is (comparable? {:account/quantity 2000M}
                       (entities/find (find-account "IRA")))
          "The trading account balance is restored"))
    (testing "The commodity account"
      (is (comparable? {:account/quantity 0M}
                       (entities/find (find-account "AAPL")))
          "The commodity account balance is restored"))
    (testing "The lot"
      (is (= 0 (entities/count {:lot/account (find-account "IRA")}))
          "The lot is deleted"))))

(deftest cannot-undo-a-purchase-if-shares-have-been-sold
  (with-context sale-context
    (trading/sell (sale-attributes))
    (is (thrown-with-msg? IllegalStateException #"Cannot undo"
                          (trading/unbuy
                            (find-transaction [(t/local-date 2016 3 2)
                                               "Purchase 100.000 shares of AAPL at 10.000"]))))))

(def ^:privat existing-sale-context
  (conj sale-context
        #:trade{:type :sale
                :entity "Personal"
                :commodity "AAPL"
                :account "IRA"
                :inventory-method :fifo
                :date (t/local-date 2017 3 2)
                :shares 25M
                :value 375M}))

; IRA balance balance before purchase: $2,000
;                      after purchase: $1,000
;                          after sale: $1,375
;                        after unsale: $1,000
(deftest ^:multi-threaded undo-a-sale
  (with-context existing-sale-context
    (let [trx (find-transaction [(t/local-date 2017 3 2)
                                 #"^Sell 25\.000 shares of AAPL"])
          ira (find-account "IRA")]
      (trading/unsell-and-propagate trx)
      (testing "The transaction"
        (is (nil? (entities/find trx))
            "The transaction cannot be retrieved after unsell"))
      (testing "The trading account"
        (is (comparable? {:account/quantity 1000M}
                         (entities/find ira))
            "The account balance is restored"))
      (testing "The affected lots"
        (is (seq-of-maps-like? [#:lot{:shares-purchased 100M
                                      :shares-owned 100M}]
                               (entities/select {:lot/account ira}))
            "The shares owned are restored")))))

(def ^:private transfer-context
  (conj sale-context
        #:account{:name "IRA 2"
                  :entity "Personal"
                  :type :asset}))

(deftest ^:multi-threaded transfer-a-commodity
  (with-context transfer-context
    (let [from-account (find-account "IRA")
          to-account (find-account "IRA 2")
          commodity (entities/find (find-commodity "AAPL")) ; reload to get the date boundaries
          [result] (trading/transfer-and-propagate
                     #:transfer{:commodity commodity
                                :from-account from-account
                                :to-account to-account
                                :shares 100M
                                :date (t/local-date 2016 4 2)})]
      (testing "The transaction"
        (is (comparable?
              #:transaction{:transaction-date (t/local-date 2016 4 2)
                            :description "Transfer 100 shares of AAPL"
                            :entity (util/->entity-ref (find-entity "Personal"))
                            :items
                            [#:transaction-item{:action :credit
                                                :account (util/->entity-ref
                                                           (entities/find-by
                                                             #:account{:name "AAPL"
                                                                       :parent from-account}))
                                                :quantity 100M
                                                :value 1000M
                                                :balance 0M}
                             #:transaction-item{:action :debit
                                                :account (util/->entity-ref
                                                           (entities/find-by
                                                             #:account{:name "AAPL"
                                                                       :parent to-account}))
                                                :quantity 100M
                                                :value 1000M
                                                :balance 100M}]}
              (entities/find (:transfer/transaction result)))
            "A transaction is created and returned"))
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:commodity (util/->entity-ref commodity)
                                      :account (util/->entity-ref to-account)
                                      :shares-owned 100M
                                      :purchase-price 10M
                                      :shares-purchased 100M
                                      :purchase-date (t/local-date 2016 3 2)}]
                               (entities/select {:lot/commodity commodity}))
            "The lot is updated to reflect the new account"))
      (testing "The originating account"
        ; Original account balance was 2,000, we bought 1,000 worth of
        ; shares of AAPL, then transfered those shares out of the account
        ; leaving 1,000 in cash
        (is (comparable? #:account{:quantity 1000M
                                   :name "IRA"}
                         (entities/find from-account))
            "The account balance reflects the cash before the transfer"))
      (testing "The destination account"
        ; No money was ever addedto the second account, so the balance
        ; is still 0
        (is (comparable? #:account{:quantity 0M
                                   :name "IRA 2"}
                         (entities/find to-account))
            "The account balance reflects the cash on hand before the transfer")))))

(deftest ^:multi-threaded split-a-commodity
  (with-context sale-context
    (let [entity (find-entity "Personal")
          ira (find-account "IRA")
          commodity (find-commodity "AAPL")
          trx-count-before (entities/count {:transaction/entity entity})
          [result] (trading/split-and-propagate
                     #:split{:commodity commodity
                             :account ira
                             :shares-gained 100M
                             :date (t/local-date 2016 3 3)})]
      (is (= 2M (:split/ratio result))
          "The split ratio is returned")
      (is (= trx-count-before
             (entities/count {:transaction/entity entity}))
          "No transaction is created")
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2016 3 2)
                                      :account (util/->entity-ref ira)
                                      :purchase-price 5M
                                      :shares-purchased 200M
                                      :shares-owned 200M}]
                               (entities/select #:lot{:commodity commodity}))
            "The lots are adjusted correctly"))
      (testing "The trading account"
        (is (comparable? {:account/quantity 1000M}
                         (entities/find ira))
            "The account balance is unchanged")))))

(def ^:private rev-split-context
  (conj base-context
        #:trade{:type :purchase
                :entity "Personal"
                :date (t/local-date 2016 3 2)
                :account "IRA"
                :commodity "AAPL"
                :shares 1500M
                :value 30000M}))

(deftest reverse-split-a-commodity
  (with-context rev-split-context
    (let [entity (find-entity "Personal")
          account (find-account "IRA")
          commodity (find-commodity "AAPL")
          count-before (entities/count {:transaction/entity entity})]
      (trading/split #:split{:date (t/local-date 2016 4 1)
                             :account account
                             :commodity commodity
                             :shares-gained -1350M})
      (is (= count-before
             (entities/count {:transaction/entity entity}))
          "No transaction is created")
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2016 3 2)
                                      :shares-purchased 150M
                                      :shares-owned 150M
                                      :purchase-price 200M}]
                               (entities/select #:lot{:account account
                                                      :commodity commodity})))))))

(def ^:private non-clean-rev-split-context
  (conj base-context
        #:trade{:type :purchase
                :entity "Personal"
                :date (t/local-date 2016 3 2)
                :account "IRA"
                :commodity "AAPL"
                :shares 700M
                :value 7000M}))

(deftest reverse-split-with-non-clean-ratio
  (with-context non-clean-rev-split-context
    (let [account (find-account "IRA")
          commodity (find-commodity "AAPL")]
      (trading/split #:split{:date (t/local-date 2016 4 1)
                             :account account
                             :commodity commodity
                             :shares-gained -600M})
      (testing "The lots after split"
        (is (seq-of-maps-like?
              [#:lot{:shares-purchased 100M
                     :shares-owned 100M}]
              (entities/select #:lot{:account account
                                     :commodity commodity}))
            "Shares are rounded correctly"))
      (testing "Selling all remaining shares"
        (trading/sell #:trade{:date (t/local-date 2016 5 1)
                              :account account
                              :commodity commodity
                              :shares 100M
                              :value 8000M})
        (is (empty?
              (entities/select
                #:lot{:account account
                      :commodity commodity
                      :shares-owned [:!= 0M]}))
            "All lots are emptied")))))
