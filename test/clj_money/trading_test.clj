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
                                            find-commodity
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.util :as util :refer [model=]]
            [clj-money.models :as models]
            [clj-money.trading :as trading]))

(use-fixtures :each reset-db)

(defn- item-by-account
  [acc transaction]
  {:pre [acc transaction]}

  (->> (:transaction/items transaction)
         (filter #(model= acc (:transaction-item/account %)))
         first))

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

(def ^:private purchase-context
  (conj base-context
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
                  :type :expense}))

(defn- purchase-attributes []
  #:trade{:commodity (find-commodity "AAPL")
          :account (find-account "IRA")
          :date (t/local-date 2016 1 2)
          :shares 100M
          :value 1000M})

(deftest purchase-a-commodity
  (with-context purchase-context
    (let [personal (find-entity "Personal")
          ira (find-account "IRA")
          commodity (find-commodity "AAPL")
          result (trading/buy (purchase-attributes))]
      (testing "The transaction"
        (is (seq-of-maps-like?
              [#:transaction{:entity (util/->model-ref personal)
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
                                                        :account (util/->model-ref ira)}
                                     #:transaction-item{:action :debit
                                                        :quantity 100M
                                                        :value 1000M
                                                        :account (util/->model-ref (:trade/commodity-account result))}]}]

              (:trade/transactions result))
            "A transaction is created and returned"))
      (testing "The commodity account"
        (is (comparable? #:account{:name "AAPL"
                                   :commodity (util/->model-ref commodity)
                                   :entity (util/->model-ref personal)
                                   :type :asset
                                   :parent (util/->model-ref ira)
                                   :system-tags #{:tradable}
                                   :price-as-of (t/local-date 2016 1 2)}
                         (models/find-by #:account{:commodity commodity
                                                   :entity personal}))
            "An account to track shares of the commodity is created"))
      (testing "The lot"
        (is (comparable? #:lot{:shares-purchased 100M
                               :shares-owned 100M
                               :purchase-price 10M
                               :purchase-date (t/local-date 2016 1 2)}
                         (:trade/lot result))))
      (testing "The trading account"
        (is (contains? (:account/system-tags (models/find ira))
                       :trading)
            "The :trading tag is added to the trading account"))
      (testing "The price"
        (is (comparable? #:price{:price 10M
                                 :trade-date (t/local-date 2016 1 2)}
                         (:trade/price result))
            "The price is returned")))))

(deftest propagate-a-purchase
  (with-context purchase-context
    (let [personal (find-entity "Personal")
          ira (find-account "IRA")
          commodity (find-commodity "AAPL")]
      (trading/buy-and-propagate (purchase-attributes))
      (testing "The commodity account"
        (is (comparable? #:account{:name "AAPL"
                                   :quantity 100M
                                   :value 1000M}
                         (models/find-by #:account{:commodity commodity
                                                   :entity personal}))
            "An account to track shares of the commodity is has propagated values"))
      (testing "The trading account"
        (is (comparable? #:account{:name "IRA"
                                   :quantity 1000M
                                   :value 1000M}
                         (models/find ira))
            "The trading account balance is updated to reflect money paid out")))))

(deftest purchase-a-commodity-with-a-fee
  (with-context purchase-context
    (let [ira (find-account "IRA")
          inv-exp (find-account "Investment Expenses")]
      (-> (purchase-attributes)
          (assoc :trade/fee 5M
                 :trade/fee-account inv-exp)
          (trading/buy-and-propagate))
      (is (= 995M (:account/quantity (models/find ira)))
          "The investment account balance reflects the fee")
      (is (= 5M (:account/quantity (models/find inv-exp)))
          "The investment expense account reflects the fee"))))

(deftest reinvest-a-dividend
  (with-context purchase-context
    (let [dividends (find-account "Dividends")
          ira (models/find (find-account "IRA"))]
      (-> #:trade{:commodity (find-commodity "AAPL")
                  :account (find-account "IRA")
                  :date (t/local-date 2016 2 2)
                  :shares 4.5M
                  :value 50M}
          (assoc :trade/dividend? true
                 :trade/dividend-account dividends)
          (trading/buy-and-propagate))
      (is (comparable? #:account{:quantity 50M
                                 :value 50M}
                       (models/find dividends))
          "The dividend account is debited for the amount of the purchase")
      (is (= (:account/quantity ira)
             (:account/quantity (models/find ira)))
          "The trading account balance is unchanged."))))

(defn- assert-invalid-purchase
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (trading/buy attr))))

(deftest purchase-requires-a-trade-date
  (with-context purchase-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :trade/date)
      {:trade/date ["Date is required"]})))

(deftest purchase-requires-a-number-of-shares
  (with-context purchase-context
    (assert-invalid-purchase
      (dissoc (purchase-attributes) :trade/shares)
      {:trade/shares ["Shares is required"]})))

(deftest purchase-requires-a-value
  (with-context purchase-context
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
          :lt-capital-gains-account (find-account "Long-term Capital Gains")
          :lt-capital-loss-account (find-account "Long-term Capital Losses")
          :st-capital-gains-account (find-account "Short-term Capital Gains")
          :st-capital-loss-account (find-account "Short-term Capital Losses")
          :inventory-method :fifo
          :date (t/local-date 2017 3 2)
          :shares 25M
          :value 375M})

(def ^:private sale-context
  (conj purchase-context
        #:trade{:type :purchase
                :entity "Personal"
                :account "IRA"
                :commodity "AAPL"
                :date (t/local-date 2016 3 2)
                :shares 100M
                :value 1000M}))

(deftest sell-a-commodity-for-a-gain-after-1-year
  (with-context sale-context
    (let [result (trading/sell (sale-attributes))
          ltcg (find-account "Long-term Capital Gains")
          aapl-acc (models/find-by #:account{:entity (find-entity "Personal")
                                             :commodity (find-commodity "AAPL")})]
      (testing "The price"
        (is (comparable? #:price{:price 15M
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
                             :description "Sell 25 shares of AAPL at 15.000"}]
              (:trade/transactions result))
            "The transaction is created and returned")
        (is (comparable?
              #:transaction-item{:action :debit
                                 :value 375M
                                 :quantity 375M}
              (item-by-account (find-account "IRA")
                               (first (:trade/transactions result))))
            "The trading account is debited the total proceeds from the purchase")
        (is (comparable? #:transaction-item{:action :credit
                                            :value 250M
                                            :quantity 25M}
                         (item-by-account aapl-acc (first (:trade/transactions result))))
            "The commodity account is credited the number of shares and purchase value of the shares."))
      (testing "The capital gains account"
        (is (comparable? #:transaction-item{:action :credit
                                            :value 125M
                                            :quantity 125M}
                         (item-by-account ltcg (first (:trade/transactions result))))
            "The capital gains account is credited the amount received above the original cost of the shares."))
      (testing "The entity"
        (let [entity (models/find-by {:entity/name "Personal"})]
          (is (model= ltcg
                      (get-in entity [:entity/settings :settings/lt-capital-gains-account]))
              "The long-term capital gains account is saved")
          (is (model= (find-account "Short-term Capital Gains")
                      (get-in entity [:entity/settings :settings/st-capital-gains-account]))
              "The short-term capital gains account is saved")
          (is (model= (find-account "Long-term Capital Losses")
                      (get-in entity [:entity/settings :settings/lt-capital-loss-account]))
              "The long-term capital losses account is saved")
          (is (model= (find-account "Short-term Capital Losses")
                      (get-in entity [:entity/settings :settings/st-capital-loss-account]))
              "The short-term capital losses account is saved"))))))

(deftest propagate-a-sale
  (with-context sale-context
    (trading/sell-and-propagate (sale-attributes))
    (testing "The commodity account"
      (is (comparable? #:account{:name "AAPL"
                                 :quantity 75M
                                 :value 1125M
                                 :price-as-of (t/local-date 2017 3 2)}
                       (models/find-by {:account/name "AAPL"}))
          "The commodity account is updated wth new value and price date"))
    (testing "The trading account"
      (is (comparable? #:account{:name "IRA"
                                 :quantity 1375M
                                 :value 1375M}
                       (models/find-by {:account/name "IRA"}))
          "The trading account is updated wth new value"))))

(deftest sell-a-commodity-for-a-gain-before-1-year
  (with-context sale-context
    (let [result (-> (sale-attributes)
                     (assoc :trade/date (t/local-date 2016 4 2))
                     trading/sell)]
      (testing "The capital gains account"
        (is (comparable? #:transaction-item{:action :credit
                                            :value 125M
                                            :quantity 125M}
                         (item-by-account (find-account "Short-term Capital Gains")
                                          (first (:trade/transactions result))))
            "The capital gains account is credited the amount received above the original cost of the shares.")))))

; sell 25 shares at $8.00 per share and $50 loss
; value before sale: $800.00
; value after sale: $600.00
(deftest sell-a-commodity-for-a-loss-after-1-year
  (with-context sale-context
    (let [result (-> (sale-attributes)
                     (assoc :trade/value 200M)
                     trading/sell)]
      (testing "The price"
        (is (comparable? #:price{:price 8M
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
                             :description "Sell 25 shares of AAPL at 8.000"}]
              (:trade/transactions result))
            "The result contains the transaction")
        (is (comparable? #:transaction-item{:action :debit
                                            :value 200M
                                            :quantity 200M}
                         (item-by-account (find-account "IRA")
                                          (first (:trade/transactions result))))
            "The trading account is debited the total proceeds from the purchase")
        (is (comparable? #:transaction-item{:action :debit
                                            :value 50M
                                            :quantity 50M}
                         (item-by-account (find-account "Long-term Capital Losses")
                                          (first (:trade/transactions result))))
            "The capital loss account is debited the cost the shares less the sale proceeds")
        (is (comparable? #:transaction-item{:action :credit
                                            :value 250M
                                            :quantity 25M}
                         (item-by-account (models/find-by #:account{:entity (find-entity "Personal")
                                                                    :commodity (find-commodity "AAPL")})
                                          (first (:trade/transactions result))))
            "The commodity account is credited the number of shares and purchase value of the shares.")))))

(deftest sell-a-commodity-for-a-loss-before-1-year
  (with-context sale-context
    (let [result (-> (sale-attributes)
                     (assoc :trade/value 200M
                            :trade/date (t/local-date 2016 4 2))
                     trading/sell)]
      (is (comparable? #:transaction-item{:action :debit
                                          :value 50M
                                          :quantity 50M}
                       (item-by-account (find-account "Short-term Capital Losses")
                                        (first (:trade/transactions result))))
          "The capital loss account is debited the cost the shares less the sale proceeds"))))

(def ^:private auto-create-context
  (conj base-context
        #:trade{:type :purchase
                :entity "Personal"
                :account "IRA"
                :commodity "AAPL"
                :date (t/local-date 2016 3 2)
                :shares 100M
                :value 1000M}))

(deftest auto-create-gains-accounts
  (with-context auto-create-context
    (trading/sell #:trade{:commodity (find-commodity "AAPL")
                          :account (find-account "IRA")
                          :inventory-method :fifo
                          :date (t/local-date 2017 3 2)
                          :shares 25M
                          :value 375M})
    (let [entity (models/find-by {:entity/name "Personal"})]
      (testing "Long-term capital gains"
        (let [account (models/find-by #:account{:entity entity
                                                :name "Long-term Capital Gains"})]
          (is (comparable? {:account/type :income}
                           account)
              "The long-term capital gains account is an income account")
          (is (model= (get-in entity
                              [:entity/settings
                               :settings/lt-capital-gains-account])
                      account)
              "The Long-term Capital Gains account is specified in the entity settings")))
      (testing "Short-term capital gains"
        (let [account (models/find-by #:account{:entity entity
                                                :name "Short-term Capital Gains"})]
          (is (comparable? {:account/type :income}
                           account)
              "The short-term capital gains account is an income account")
          (is (model= (get-in entity
                              [:entity/settings
                               :settings/st-capital-gains-account])
                      account)
              "The Short-term Capital Gains account id is placed in the entity settings")))
      (testing "Long-term capital losses"
        (let [account (models/find-by #:account{:entity entity
                                                :name "Long-term Capital Losses"})]
          (is (comparable? {:account/type :expense}
                           account)
              "The long-term capital losses account is an expense account")
          (is (model= (get-in entity
                              [:entity/settings
                               :settings/lt-capital-loss-account])
                      account)
              "The Long-term Capital Losses account id is placed in the entity settings")))
      (testing "Short-term capital losses"
        (let [account (models/find-by #:account{:entity entity
                                                :name "Short-term Capital Losses"})]
          (is (comparable? {:account/type :expense}
                           account)
              "The short-term capital losses account is an expense account")
          (is (model= (get-in entity
                              [:entity/settings
                               :settings/st-capital-loss-account])
                      account)
              "The Short-term Capital Losses account id is placed in the entity settings"))))))

(deftest sell-a-commodity-with-a-fee
  (with-context sale-context
    (trading/sell (assoc (sale-attributes)
                         :trade/fee 5M
                         :trade/fee-account (find-account "Investment Expenses")))
    ; Opening balance             $2,000
    ; Purchase AAPL     -1,000 -> $1,000
    ; Sell AAPL          + 375 -> $1,375
    ; less the fee        -  5 -> $1,370
    (is (comparable? {:account/quantity 1370M}
                     (models/find (find-account "IRA")))
        "The investment account balance reflects the fee")
    (is (comparable? {:account/quantity 5M}
                     (models/find (find-account "Investment Expenses")))
        "The investment fee account balance reflects the fee")))

(defn- assert-invalid-sale
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (trading/sell attr))))

(deftest sales-requires-a-trade-date
  (with-context sale-context
    (assert-invalid-sale
      (dissoc (sale-attributes) :trade/date)
      {:trade/date ["Date is required"]})))

(deftest sales-requires-a-number-of-shares
  (with-context sale-context
    (assert-invalid-sale
      (dissoc (sale-attributes) :trade/shares)
      {:trade/shares ["Shares is required"]})))

(deftest sales-requires-a-value
  (with-context sale-context
    (assert-invalid-sale
      (dissoc (sale-attributes) :trade/value)
      {:trade/value ["Value is required"]})))
 
; Selling a commodity updates a lot record
; (FILO updates the most recent, FIFO updates the oldest)

(def ^:private multi-lot-context
  (conj purchase-context
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
          :value 1500M
          :lt-capital-gains-account (find-account "Long-term Capital Gains")
          :st-capital-gains-account (find-account "Short-term Capital Gains")
          :lt-capital-loss-account (find-account "Long-term Capital Losses")
          :st-capital-loss-account (find-account "Short-term Capital Losses")})

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
                             (models/select #:lot{:commodity commodity
                                                  :account ira}))
          "Shares are sold from the most recent lot"))))

(deftest fifo-sale
  (with-context multi-lot-context
    (let [commodity (find-commodity "AAPL")
          ira (find-account "IRA")]
      (-> (multi-lot-sale-attributes ira commodity)
          (assoc :trade/inventory-method :fifo)
          trading/sell)
      (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2015 3 2)
                                    :shares-purchased 100M
                                    :shares-owned 50M
                                    :purchase-price 10M}
                              #:lot{:purchase-date (t/local-date 2016 3 2)
                                    :shares-purchased 100M
                                    :shares-owned 100M
                                    :purchase-price 20M}]
                             (models/select #:lot{:commodity commodity
                                                  :account ira}))
          "Shares are sold from the earliest lot"))))

(deftest undo-a-purchase
  (with-context sale-context
    (trading/unbuy-and-propagate
      (find-transaction [(t/local-date 2016 3 2)
                         "Purchase 100 shares of AAPL at 10.000"] ))
    ; TODO Should we delete the price that was created?
    (testing "The trading account"
      (is (comparable? {:account/quantity 2000M}
                       (models/find (find-account "IRA")))
          "The trading account balance is restored"))
    (testing "The commodity account"
      (is (comparable? {:account/quantity 0M}
                       (models/find (find-account "AAPL")))
          "The commodity account balance is restored"))
    (testing "The lot"
      (is (= 0 (models/count {:lot/account (find-account "IRA")}))
          "The lot is deleted"))))

(deftest cannot-undo-a-purchase-if-shares-have-been-sold
  (with-context sale-context
    (trading/sell (sale-attributes))
    (is (thrown-with-msg? IllegalStateException #"Cannot undo"
                          (trading/unbuy
                            (find-transaction [(t/local-date 2016 3 2)
                                               "Purchase 100 shares of AAPL at 10.000"]))))))

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
(deftest undo-a-sale
  (with-context existing-sale-context
    (let [trx (find-transaction [(t/local-date 2017 3 2)
                                 "Sell 25 shares of AAPL at 15.000"])
          ira (find-account "IRA")]
      (trading/unsell trx)
      (testing "The transaction"
        (is (nil? (models/find trx))
            "The transaction cannot be retrieved after unsell"))
      (testing "The trading account"
        (is (comparable? {:account/quantity 1000M}
                         (models/find ira))
            "The account balance is restored"))
      (testing "The affected lots"
        (is (seq-of-maps-like? [#:lot{:shares-purchased 100M
                                      :shares-owned 100M}]
                               (models/select {:lot/account ira}))
            "The shares owned are restored")))))

(def ^:private transfer-context
  (conj sale-context
        #:account{:name "IRA 2"
                  :entity "Personal"
                  :type :asset}))

(deftest transfer-a-commodity
  (with-context transfer-context
    (let [from-account (find-account "IRA")
          to-account (find-account "IRA 2")
          commodity (models/find (find-commodity "AAPL")) ; reload to get the date boundaries
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
                            :entity (util/->model-ref (find-entity "Personal"))
                            :items [#:transaction-item{:action :credit
                                                       :quantity 100M
                                                       :value 1000M
                                                       :balance 0M
                                                       :account (util/->model-ref
                                                                  (models/find-by #:account{:name "AAPL"
                                                                                            :parent from-account}))}
                                    #:transaction-item{:action :debit
                                                       :quantity 100M
                                                       :value 1000M
                                                       :balance 100M
                                                       :account (util/->model-ref
                                                                  (models/find-by #:account{:name "AAPL"
                                                                                            :parent to-account}))}]}
              (models/find (:transfer/transaction result)))
            "A transaction is created and returned"))
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:commodity (util/->model-ref commodity)
                                      :account (util/->model-ref to-account)
                                      :shares-owned 100M
                                      :purchase-price 10M
                                      :shares-purchased 100M
                                      :purchase-date (t/local-date 2016 3 2)}]
                               (models/select {:lot/commodity commodity}))
            "The lot is updated to reflect the new account"))
      (testing "The originating account"
        ; Original account balance was 2,000, we bought 1,000 worth of
        ; shares of AAPL, then transfered those shares out of the account
        ; leaving 1,000 in cash
        (is (comparable? #:account{:quantity 1000M
                                   :name "IRA"}
                         (models/find from-account))
            "The account balance reflects the cash before the transfer"))
      (testing "The destination account"
        ; No money was ever addedto the second account, so the balance
        ; is still 0
        (is (comparable? #:account{:quantity 0M
                                   :name "IRA 2"}
                         (models/find to-account))
            "The account balance reflects the cash on hand before the transfer")))))

(deftest split-a-commodity
  (with-context sale-context
    (let [ira (find-account "IRA")
          commodity (find-commodity "AAPL")
          commodity-account (models/find-by #:account{:name "AAPL"
                                                      :parent ira})
          result (trading/split-and-propagate
                   #:split{:commodity commodity
                           :account ira
                           :shares-gained 100M
                           :date (t/local-date 2016 3 3)})]
      (is (= 2M (:split/ratio result))
          "The split ratio is returned")
      (testing "The transaction"
        (is (comparable? #:transaction{:entity (:commodity/entity commodity)
                                       :transaction-date (t/local-date 2016 3 3)
                                       :description "Split shares of AAPL 2 for 1"
                                       :value 0M
                                       :items [#:transaction-item{:action :debit
                                                                  :account (util/->model-ref commodity-account)
                                                                  :quantity 100M
                                                                  :balance 200M
                                                                  :value 0M}]}
                         (:split/transaction result))
            "The result contains the transaction that was created"))
      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2016 3 2)
                                      :account (util/->model-ref ira)
                                      :purchase-price 5M
                                      :shares-purchased 200M
                                      :shares-owned 200M}]
                               (models/select #:lot{:commodity commodity}))
            "The lots are adjusted correctly"))
      (testing "The trading account"
        (is (comparable? {:account/quantity 1000M}
                         (models/find ira))
            "The account balance is unchanged")))))

(def ^:private rev-split-context
  (conj purchase-context
        #:trade{:type :purchase
                :entity "Personal"
                :date (t/local-date 2016 3 2)
                :account "IRA"
                :commodity "AAPL"
                :shares 1500M
                :value 30000M}))

(deftest reverse-split-a-commodity
  (with-context rev-split-context
    (let [account (find-account "IRA")
          commodity (find-commodity "AAPL")
          result (trading/split #:split{:date (t/local-date 2016 4 1)
                                        :account account
                                        :commodity commodity
                                        :shares-gained -1350M})]
      (testing "The transaction"
        (is (comparable? #:transaction{:description "Split shares of AAPL 1 for 10"
                                       :items [#:transaction-item{:action :credit
                                                                  :quantity 1350M}]}
                         (:split/transaction result))))

      (testing "The lots"
        (is (seq-of-maps-like? [#:lot{:purchase-date (t/local-date 2016 3 2)
                                      :shares-purchased 150M
                                      :shares-owned 150M
                                      :purchase-price 200M}]
                               (models/select #:lot{:account account
                                                    :commodity commodity})))))))
