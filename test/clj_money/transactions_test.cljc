(ns clj-money.transactions-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :as dates]
            [clj-money.decimal :refer [d]]
            [clj-money.util :as util]
            [clj-money.transactions :as trx]))

(deftest identify-an-accountified-transaction
  (is (trx/accountified? {:transaction/quantity (d -10)
                          :transaction/account {:id :checking}
                          :transaction/other-account {:id :groceries}})
      "A transaction with :account and :other-account attributes is accountified")
  (is (not (trx/accountified? {:transaction/items []}))
      "A transaction with :items is not accountified"))

(defn- index
  [& inputs]
  (let [[->key & entities] (if (map? (first inputs))
                             (cons util/->entity-ref inputs)
                             inputs)]
    (->> entities
         (map (juxt ->key identity))
         (into {}))))

(def ^:private accounts
  (index :id
         {:id :checking
          :account/name "Checking"
          :account/type :asset}
         {:id :groceries
          :account/name "Groceries"
          :account/type :expense}
         {:id :savings
          :account/name "Savings"
          :account/type :asset}
         {:id :credit-card
          :account/name "Credit Card"
          :account/type :liability}))

(deftest accountify-a-transaction
  (let [trx #:transaction{:transaction-date "2020-01-01"
                          :description "ACME Store"
                          :memo "transaction memo"
                          :items [{:id 1
                                   :transaction-item/account (accounts :checking)
                                   :transaction-item/memo "checking memo" ; NOTE: these memos are lost
                                   :transaction-item/action :credit
                                   :transaction-item/quantity (d 10)}
                                  {:id 2
                                   :transaction-item/account (accounts :groceries)
                                   :transaction-item/memo "groceries memo"
                                   :transaction-item/action :debit
                                   :transaction-item/quantity (d 10)}]}
        expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :item {:id 1}
                               :account (accounts :checking)
                               :other-item {:id 2} :other-account (accounts :groceries)
                               :quantity (d -10)}]
    (is (= expected (trx/accountify trx (accounts :checking))))))

(deftest unaccountify-a-transaction
  (testing "A whole transaction"
    (let [expected #:transaction{:transaction-date "2020-01-01"
                                 :description "ACME Store"
                                 :memo "transaction memo"
                                 :items [{:id 1
                                          :transaction-item/account {:id :checking}
                                          :transaction-item/action :credit
                                          :transaction-item/quantity (d 10)}
                                         {:id 2
                                          :transaction-item/account {:id :groceries}
                                          :transaction-item/action :debit
                                          :transaction-item/quantity (d 10)}]}
          simple #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :item {:id 1}
                               :account {:id :checking}
                               :other-item {:id 2}
                               :other-account {:id :groceries}
                               :quantity (d -10)}]
      (is (= expected (trx/unaccountify simple (comp accounts :id))))
      (testing "two asset accounts"
        (is (= (assoc-in expected [:transaction/items 1 :transaction-item/account] {:id :savings})
               (trx/unaccountify (assoc simple :transaction/other-account {:id :savings})
                                 (comp accounts :id)))))
      (testing "one asset, one liability"
        (is (= (assoc-in expected [:transaction/items 1 :transaction-item/account] {:id :credit-card})
               (trx/unaccountify (assoc simple :transaction/other-account {:id :credit-card})
                                 (comp accounts :id)))))))
  (testing "A partial transaction (for editing)"
    (is (= #:transaction{:transaction-date "2020-01-01"
                         :items [#:transaction-item{:account {:id :checking}
                                                    :action :credit}]}
           (trx/unaccountify #:transaction{:transaction-date "2020-01-01"
                                           :account {:id :checking}}
                             (comp accounts :id))))))

(deftest entryfy-a-transaction
  (let [transaction #:transaction{:transaction-date "2020-01-01"
                                  :description "ACME Store"
                                  :memo "transaction memo"
                                  :items [#:transaction-item{:account {:id 1}
                                                             :memo "checking memo"
                                                             :action :credit
                                                             :quantity (d 10)}
                                          #:transaction-item{:account {:id 2}
                                                             :memo "groceries memo"
                                                             :action :debit
                                                             :quantity (d 10)}]}
        expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [#:transaction-item{:account {:id 1}
                                                          :memo "checking memo"
                                                          :credit-quantity (d 10)
                                                          :debit-quantity nil}
                                       #:transaction-item{:account {:id 2}
                                                          :memo "groceries memo"
                                                          :credit-quantity nil
                                                          :debit-quantity (d 10)}
                                       {}]}]
    (is (= expected (trx/entryfy transaction)))))

(deftest unentryfy-a-transaction
  (let [expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [#:transaction-item{:account {:id 1}
                                                          :memo "checking memo"
                                                          :action :credit
                                                          :quantity (d 10)
                                                          :value (d 10)}
                                       #:transaction-item{:account {:id 2}
                                                          :memo "groceries memo"
                                                          :action :debit
                                                          :quantity (d 10)
                                                          :value (d 10)}]}
        transaction #:transaction{:transaction-date "2020-01-01"
                                  :description "ACME Store"
                                  :memo "transaction memo"
                                  :items [#:transaction-item{:account {:id 1}
                                                             :memo "checking memo"
                                                             :credit-quantity (d 10)
                                                             :debit-quantity nil}
                                          #:transaction-item{:account {:id 2}
                                                             :memo "groceries memo"
                                                             :credit-quantity nil
                                                             :debit-quantity (d 10)}
                                          {}]}]
    (is (= expected (trx/unentryfy transaction)))
    (testing "transaction contains 'deleted' items"
      (is (= expected
             (trx/unentryfy (update-in transaction
                                       [:transaction/items]
                                       conj
                                       {:transaction-item/account {:id 3}})))))))

(deftest simplifiability
  (is (trx/can-accountify?
       {:transaction/items [#:transaction-item{:action :debit
                                               :account {:id 1}
                                               :quantity (d 10)}
                            #:transaction-item{:action :credit
                                               :account {:id 2}
                                               :quantity (d 10)}]})
      "A two-item transaction can be simplified")
  (is (trx/can-accountify?
       {:transaction/items [#:transaction-item{:action :debit
                                               :account {:id 1}
                                               :quantity (d 10)}
                            #:transaction-item{:action :credit
                                               :account {:id 2}
                                               :quantity (d 10)}
                            {}]})
      "A transaction with two full items and one emtpy item can be simplified")
  (is (not (trx/can-accountify?
            {:transaction/items [#:transaction-item{:action :debit
                                                    :account {:id 1}
                                                    :quantity (d 10)}
                                 #:transaction-item{:action :credit
                                                    :account {:id 2}
                                                    :quantity (d 6)}
                                 #:transaction-item{:action :credit
                                                    :account {:id 3}
                                                    :quantity (d 4)}]}))
      "A transaction with more than two non-empty items cannot be simplified"))

(deftest ensure-an-empty-item
  (let [expected {:transaction/items [#:transaction-item{:credit-quantity (d 10)}
                                      #:transaction-item{:debit-quantity (d 10)}
                                      {}]}]
    (testing "purely empty items"
      (is (= expected
             (trx/ensure-empty-item {:transaction/items [#:transaction-item{:credit-quantity (d 10)}
                                                         #:transaction-item{:debit-quantity (d 10)}]}))
          "An empty item is added when there are no empty items")
      (is (= expected
             (trx/ensure-empty-item {:transaction/items [#:transaction-item{:credit-quantity (d 10)}
                                                         {}
                                                         #:transaction-item{:debit-quantity (d 10)}
                                                         {}]}))
          "An extra empty item is removed")
      (is (= expected
             (trx/ensure-empty-item {:transaction/items [#:transaction-item{:credit-quantity (d 10)}
                                                         #:transaction-item{:debit-quantity (d 10)}
                                                         {}]}))
          "No action is taken if one empty item is present"))))

(def ^:private trading-context
  {:accounts (index {:id :401k
                     :account/name "401k"
                     :account/type :asset
                     :account/system-tags #{:trading}
                     :account/commodity {:id :usd}}
                    {:id :aapl
                     :account/parent-id 1
                     :account/name "AAPL"
                     :account/type :asset
                     :account/system-tags #{:tradable}
                     :account/commodity {:id :aapl}})
   :commodities (index {:id :usd
                        :commodity/name "USD"
                        :commodity/symbol "USD"
                        :commodity/type :currency}
                       {:id :aapl
                        :commodity/name "Apple, Ince."
                        :commodity/symbol "AAPL"
                        :commodity/type :stock
                        :commodity/exchange :nasdaq})})

(def ^:private find-account-with-commodity
  (comp (->> (vals (:accounts trading-context))
             (map (juxt :account/commodity
                        identity))
             (into {}))))

(deftest tradify-a-partial-transaction
  (let [tradified (trx/tradify #:transaction{:transaction-date "2020-01-01"
                                             :items [#:transaction-item{:account {:id :401k}
                                                                        :action :credit}]}
                               {:find-account (:accounts trading-context)
                                :find-commodity (:commodities trading-context)})]
    (is (= "2020-01-01" (:trade/trade-date tradified)) "The trade-date is taken from transaction-date")
    (is (util/entity= {:id :401k} (:trade/account tradified)) "The account is taken from the item for the trading account")
    (is (= :buy (:trade/action tradified)) "The action defauls to buy")
    (is (nil? (:trade/commodity tradified)) "The commodity id is nil")
    (is (nil? (:trade/shares tradified)) "The shares is nil")))

(deftest tradify-a-buy-transaction
  (let [standard #:transaction{:transaction-date "2020-01-01"
                               :items [#:transaction-item{:account {:id :401k}
                                                          :action :credit
                                                          :quantity (d 100)}
                                       #:transaction-item{:account {:id :aapl}
                                                          :action :debit
                                                          :quantity (d 100)}]}
        tradified #:trade{:trade-date "2020-01-01"
                          :shares (d 100)
                          :account {:id :401k}
                          :commodity {:id :aapl}
                          :action :buy}]
    (testing "a standard transaction can be converted to a trade transaction"
      (is (= tradified
             (-> (trx/tradify standard
                              {:find-account (:accounts trading-context)
                               :find-commodity (:commodities trading-context)})
                 (update-in [:trade/account] util/->entity-ref)
                 (update-in [:trade/commodity] util/->entity-ref)))))
    (testing "a trade transaction can be converted to a standard"
      (is (= standard
             (update-in (trx/untradify tradified
                                       {:find-account-with-commodity find-account-with-commodity})
                        [:transaction/items]
                        (fn [items]
                          (map #(update-in %
                                           [:transaction-item/account]
                                           util/->entity-ref)
                               items))))))))

(deftest tradify-a-sell-transaction
  (let [standard #:transaction{:transaction-date "2020-01-01"
                               :items [#:transaction-item{:account {:id :aapl}
                                                          :action :credit
                                                          :quantity (d 100)}
                                       #:transaction-item{:account {:id :401k}
                                                          :action :debit
                                                          :quantity (d 100)}]}
        tradified #:trade{:trade-date "2020-01-01"
                          :shares (d 100)
                          :account {:id :401k}
                          :commodity {:id :aapl}
                          :action :sell}]
    (testing "a standard transaction can be converted to a trade transaction"
      (is (= tradified
             (-> (trx/tradify standard
                              {:find-account (:accounts trading-context)
                               :find-commodity (:commodities trading-context)})
                 (update-in [:trade/account] util/->entity-ref)
                 (update-in [:trade/commodity] util/->entity-ref)))))
    (testing "a trade transaction can be converted to a standard"
      (is (= standard
             (update-in (trx/untradify tradified
                                       {:find-account-with-commodity find-account-with-commodity})
                        [:transaction/items]
                        (fn [items]
                          (mapv #(update-in % [:transaction-item/account] util/->entity-ref)
                                items))))))))

(deftest summarize-some-items
  (let [items [{:transaction-item/polarized-quantity (d 100)
                :transaction/transaction-date (t/local-date 2016 1 2)}
               {:transaction-item/polarized-quantity (d 101)
                :transaction/transaction-date (t/local-date 2016 1 16)}
               {:transaction-item/polarized-quantity (d 102)
                :transaction/transaction-date (t/local-date 2016 3 1)}]
        expected [{:start-date (t/local-date 2016 1 1)
                   :end-date (t/local-date 2016 1 31)
                   :quantity (d 201)}
                  {:start-date (t/local-date 2016 2 1)
                   :end-date (t/local-date 2016 2 29)
                   :quantity (d 0)}
                  {:start-date (t/local-date 2016 3 1)
                   :end-date (t/local-date 2016 3 31)
                   :quantity (d 102)}
                  {:start-date (t/local-date 2016 4 1)
                   :end-date (t/local-date 2016 4 30)
                   :quantity (d 0)}]]
    (is (= expected
           (trx/summarize-items {:since (t/local-date 2016 1 1)
                                 :as-of (t/local-date 2016 4 30)
                                 :period [1 :month]}
                                items)))))

(deftest calc-the-value-of-a-transaction
  (is (= (d 100)
         (trx/value
          #:transaction{:items [#:transaction-item{:quantity (d 100)
                                                   :value (d 100)
                                                   :action :credit
                                                   :account {:id :checking}}
                                #:transaction-item{:quantity (d 100)
                                                   :value (d 100)
                                                   :action :debit
                                                   :account {:id :groceries}}]}))
      "The value is the sum of credits (or debits)")
  (is (nil?
       (trx/value
        #:transaction{:items [#:transaction-item{:quantity (d 101)
                                                 :value (d 101)
                                                 :action :credit
                                                 :account {:id :checking}}
                              #:transaction-item{:quantity (d 100)
                                                 :value (d 100)
                                                 :action :debit
                                                 :account {:id :groceries}}]}))
      "The value is nil (undeterminable) if the credits and debits do not match"))

(def ^:private salary           {:id "salary"           :account/type :income})
(def ^:private other-income     {:id "other income"     :account/type :income})
(def ^:private checking         {:id "checking"         :account/type :asset})
(def ^:private four-oh-one-k    {:id "401k"             :account/type :asset})
(def ^:private aapl             {:id "aapl"             :account/type :asset})
(def ^:private groceries        {:id "groceries"        :account/type :expense})
(def ^:private supplements      {:id "supplements"      :account/type :expense})
(def ^:private insurance        {:id "insurance"        :account/type :expense})
(def ^:private health-insurance {:id "health insurance" :account/type :expense})
(def ^:private fit              {:id "fit"              :account/type :expense})
(def ^:private medicare         {:id "medicare"         :account/type :expense})
(def ^:private social-security  {:id "social security"  :account/type :expense})

(def ^:private simple-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/quantity (d 100)
   :transaction/debit-account groceries
   :transaction/credit-account checking})

(def ^:private simple-bilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:transaction-item/value (d 100)
                        :transaction-item/debit-account groceries
                        :transaction-item/credit-account checking
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 100)}
                         {:account-item/action :credit
                          :account-item/quantity (d -100)}]}]})

(def ^:private simple-unilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:transaction-item/quantity (d 100)
                        :transaction-item/value (d 100)
                        :transaction-item/action :debit
                        :transaction-item/account groceries}
                       {:transaction-item/quantity (d 100)
                        :transaction-item/value (d 100)
                        :transaction-item/action :credit
                        :transaction-item/account checking}]})

(def ^:private complex-bilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:id 1
                        :transaction-item/value (d 100)
                        :transaction-item/debit-account groceries
                        :transaction-item/credit-account checking
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 100)}
                         {:account-item/action :credit
                          :account-item/quantity (d -100)}]}
                       {:id 2
                        :transaction-item/value (d 20)
                        :transaction-item/debit-account supplements
                        :transaction-item/credit-account checking
                        :transaction-item/action-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 20)}
                         {:account-item/action :credit
                          :account-item/quantity (d -20)}]}]})

(def ^:private complex-unilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:ids #{1 2}
                        :transaction-item/quantity (d 120)
                        :transaction-item/value (d 120)
                        :transaction-item/account checking
                        :transaction-item/action :credit}
                       {:ids #{1}
                        :transaction-item/quantity (d 100)
                        :transaction-item/value (d 100)
                        :transaction-item/account groceries
                        :transaction-item/action :debit}
                       {:ids #{2}
                        :transaction-item/quantity (d 20)
                        :transaction-item/value (d 20)
                        :transaction-item/account supplements
                        :transaction-item/action :debit}]})

;       debit       credit
;       --------    ------
; 4,225 checking    salary
; 1,400 fit         salary
;   775 401k        salary
;   400 soc. sec.   salary
;   200 health ins. salary
;   100 medicare    salary
;   100 insurance   other income
;    25 checking    other income
; -----
; 7,225
(def ^:private very-complex-bilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Paycheck"
   :transaction/entity {:id "personal"}
   :transaction/items [{:transaction-item/value (d 100)
                        :transaction-item/debit-account insurance
                        :transaction-item/credit-account other-income
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 100)
                          :account-item/memo "group term life insurance"}
                         {:account-item/action :credit
                          :account-item/quantity (d 100)
                          :account-item/memo "group term life insurance"}]}
                       {:transaction-item/value (d 4250)
                        :transaction-item/debit-account checking
                        :transaction-item/credit-account salary
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 4250)}
                         {:account-item/action :credit
                          :account-item/quantity (d 4250)}]}
                       {:transaction-item/value (d 1400)
                        :transaction-item/debit-account fit
                        :transaction-item/credit-account salary
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 1400)}
                         {:account-item/action :credit
                          :account-item/quantity (d 1400)}]}
                       {:transaction-item/value (d 775)
                        :transaction-item/debit-account four-oh-one-k
                        :transaction-item/credit-account salary
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 775)}
                         {:account-item/action :credit
                          :account-item/quantity (d 775)}]}
                       {:transaction-item/value (d 400)
                        :transaction-item/debit-account social-security
                        :transaction-item/credit-account salary
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 400)}
                         {:account-item/action :credit
                          :account-item/quantity (d 400)}]}
                       {:transaction-item/value (d 200)
                        :transaction-item/debit-account health-insurance
                        :transaction-item/credit-account salary
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 200)}
                         {:account-item/action :credit
                          :account-item/quantity (d 200)}]}
                       {:transaction-item/value (d 75)
                        :transaction-item/debit-account medicare
                        :transaction-item/credit-account salary
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 75)}
                         {:account-item/action :credit
                          :account-item/quantity (d 75)}]}
                       {:transaction-item/value (d 25)
                        :transaction-item/debit-account medicare
                        :transaction-item/credit-account other-income
                        :transaction-item/account-items
                        [{:account-item/action :debit
                          :account-item/quantity (d 25)}
                         {:account-item/action :credit
                          :account-item/quantity (d 25)
                          :account-item/memo "cell phone reimbursement"}]}]})

; 4,250 checking
; 1,400 fit
;   775 401k
;   400 social security
;   200 health insurance
;   100 medicare
;   100 insurance (life insurance)
; -----
; 7,225 debit
;
; 7,100 salary
;   100 other income (life insurance)
;    25 other income (cell phone reimbursement)
; -----
; 7,225 credit
(def ^:private very-complex-unilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Paycheck"
   :transaction/entity {:id "personal"}
   :transaction/items [{:transaction-item/quantity (d 4250)
                        :transaction-item/value (d 4250)
                        :transaction-item/account checking
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 1400)
                        :transaction-item/value (d 1400)
                        :transaction-item/account fit
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 775)
                        :transaction-item/value (d 775)
                        :transaction-item/account four-oh-one-k
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 400)
                        :transaction-item/value (d 400)
                        :transaction-item/account social-security
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 200)
                        :transaction-item/value (d 200)
                        :transaction-item/account health-insurance
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 100)
                        :transaction-item/value (d 100)
                        :transaction-item/account medicare
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 100)
                        :transaction-item/value (d 100)
                        :transaction-item/account insurance
                        :transaction-item/action :debit
                        :transaction-item/memo "group term life insurance"}

                       {:transaction-item/quantity (d 7100)
                        :transaction-item/value (d 7100)
                        :transaction-item/account salary
                        :transaction-item/action :credit}
                       {:transaction-item/quantity (d 100)
                        :transaction-item/value (d 100)
                        :transaction-item/account other-income
                        :transaction-item/action :credit
                        :transaction-item/memo "group term life insurance"}
                       {:transaction-item/quantity (d 25)
                        :transaction-item/value (d 25)
                        :transaction-item/account other-income
                        :transaction-item/action :credit
                        :transaction-item/memo "cell phone reimbursement"}]})

(def ^:private unilateral-trading-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Purchase 10 shares of AAPL at 10.0000"
   :transaction/entity {:id "personal"}
   :transaction/items [{:transaction-item/action :credit
                        :transaction-item/account four-oh-one-k
                        :transaction-item/quantity (d 1000)
                        :transaction-item/value (d 1000)}
                       {:transaction-item/action :debit
                        :transaction-item/account aapl
                        :transaction-item/quantity (d 10)
                        :transaction-item/value (d 1000)}]})

(def ^:private bilateral-trading-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Purchase 10 shares of AAPL at 10.0000"
   :transaction/entity {:id "personal"}
   :transaction/items [{:transaction-item/credit-account four-oh-one-k
                        :transaction-item/debit-account aapl
                        :transaction-item/debit-quantity (d 10)
                        :transaction-item/value (d 1000)}]})

(defn- comparable-trx
  [trx]
  (update-in trx [:transaction/items] set))

(deftest convert-a-transaction-into-a-bilateral
  (testing "a bilateral transaction"
    (is (= simple-bilateral-trx
           (trx/->bilateral simple-bilateral-trx))
        "A bilateral transaction is return unchanged"))
  (testing "a simple transaction"
    (is (= simple-bilateral-trx
           (trx/->bilateral simple-trx))))
  (testing "a simple unilateral transaction"
    (is (= simple-bilateral-trx
           (trx/->bilateral simple-unilateral-trx))))
  (testing "a complex unilateral transaction"
    (is (= complex-bilateral-trx
           (trx/->bilateral complex-unilateral-trx))))
  (testing "a complex unilateral transaction, swap debits and credits"
    (is (= (-> complex-bilateral-trx
               (update-in [:transaction/items]
                          (fn [items]
                            (mapv (fn [{:as i :transaction-item/keys [debit-account credit-account]}]
                                    (assoc i
                                           :transaction-item/debit-account credit-account
                                           :transaction-item/credit-account debit-account))
                                  items))))
           (-> complex-unilateral-trx
               (assoc-in [:transaction/items 0 :transaction-item/action] :debit)
               (assoc-in [:transaction/items 1 :transaction-item/action] :credit)
               (assoc-in [:transaction/items 2 :transaction-item/action] :credit)
               trx/->bilateral))))
  (testing "a very complex unilateral transaction"
    (is (= (comparable-trx very-complex-bilateral-trx)
           (comparable-trx (trx/->bilateral very-complex-unilateral-trx)))))
  (testing "a unilateral trading transaction"
    (is (= (comparable-trx bilateral-trading-trx)
           (comparable-trx (trx/->bilateral unilateral-trading-trx))))))

(deftest convert-a-transaction-into-a-unilateral
  (testing "a unilateral transaction"
    (is (= simple-unilateral-trx
           (trx/->unilateral simple-unilateral-trx))
        "A unilateral transaction is returned unchanged"))
  (testing "a simple transaction"
    (is (= simple-unilateral-trx
           (trx/->unilateral simple-trx))))
  (testing "a simple bilateral transaction"
    (is (= simple-unilateral-trx
           (trx/->unilateral simple-bilateral-trx))))
  (testing "a complex bilateral transaction"
    (is (= (comparable-trx complex-unilateral-trx)
           (comparable-trx (trx/->unilateral complex-bilateral-trx)))))
  (testing "a very complex bilateral transaction"
    (is (= (comparable-trx very-complex-unilateral-trx)
           (comparable-trx (trx/->unilateral very-complex-bilateral-trx)))))
  (testing "a bilateral trading transaction"
    (is (= (comparable-trx unilateral-trading-trx)
           (comparable-trx (trx/->unilateral bilateral-trading-trx))))))

(deftest simplify-a-transaction
  (testing "a bilateral transaction with one item"
    (is (= simple-trx
           (trx/simplify simple-bilateral-trx))))
  (testing "a bilateral transaction with multiple items"
    (is (nil? (trx/simplify complex-bilateral-trx))
        "cannot be created")))

(def ^:private account-types
  {"checking"    :asset
   "groceries"   :expense
   "supplements" :expense})

(defn- append-type
  [{:as account :keys [id]}]
  (assoc account :account/type (account-types id)))

(defn- append-account-types
  [items]
  (map (fn [item]
         (-> item
             (update-in [:transaction-item/debit-account]
                        append-type)
             (update-in [:transaction-item/credit-account]
                        append-type)))
       items))

(defn- append-item-ids
  [items]
  (->> items
       (interleave (range))
       (partition 2)
       (map (fn [[idx item]]
              (assoc item :id (inc idx))))))

(deftest produce-account-items-for-a-transaction
  (testing "A simple transaction"
    (is (= [{:account-item/account {:id "groceries"
                                    :account/type :expense}
             :account-item/transaction-item {:id 1}
             :account-item/quantity (d 100)}
            {:account-item/account {:id "checking"
                                    :account/type :asset}
             :account-item/transaction-item {:id 1}
             :account-item/quantity (d -100)}]
           (->> (:transaction/items simple-bilateral-trx)
                append-account-types
                append-item-ids
                trx/make-account-items
                (map util/remove-nils)))))
  (testing "A complex transaction"
    (is (= [{:account-item/account {:id "groceries"
                                    :account/type :expense}
             :account-item/transaction-item {:id 1}
             :account-item/quantity (d 100)}
            {:account-item/account {:id "checking"
                                    :account/type :asset}
             :account-item/transaction-item {:id 1}
             :account-item/quantity (d -100)}
            {:account-item/account {:id "supplements"
                                    :account/type :expense}
             :account-item/transaction-item {:id 2}
             :account-item/quantity (d 20)}
            {:account-item/account {:id "checking"
                                    :account/type :asset}
             :account-item/transaction-item {:id 2}
             :account-item/quantity (d -20)}]
           (->> (:transaction/items complex-bilateral-trx)
                append-account-types
                trx/make-account-items
                (map util/remove-nils))))))
