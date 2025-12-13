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

(deftest expand-a-transaction
  (let [expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [#:transaction-item{:account {:id :groceries}
                                                          :action :debit
                                                          :quantity (d 10)}
                                       #:transaction-item{:account {:id :checking}
                                                          :action :credit
                                                          :quantity (d 10)}]}
        simple #:transaction{:transaction-date "2020-01-01"
                             :description "ACME Store"
                             :memo "transaction memo"
                             :debit-account {:id :groceries}
                             :credit-account {:id :checking}
                             :quantity (d 10)}]
    (is (= expected (trx/expand simple))
        "A simple transaction is expanded")
    (is (= expected (trx/expand expected))
        "An expanded transaction is returned as-is")))

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

(def ^:private simple-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/quantity (d 100)
   :transaction/debit-account {:id "groceries"}
   :transaction/credit-account {:id "checking"}})

(def ^:private simple-bilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:transaction-item/quantity (d 100)
                        :transaction-item/debit-account {:id "groceries"}
                        :transaction-item/credit-account {:id "checking"}}]})

(def ^:private simple-unilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:transaction-item/quantity (d 100)
                        :transaction-item/action :debit
                        :transaction-item/account {:id "groceries"}}
                       {:transaction-item/quantity (d 100)
                        :transaction-item/action :credit
                        :transaction-item/account {:id "checking"}}]})

(def ^:private complex-bilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:transaction-item/quantity (d 100)
                        :transaction-item/debit-account {:id "groceries"}
                        :transaction-item/credit-account {:id "checking"}}
                       {:transaction-item/quantity (d 20)
                        :transaction-item/debit-account {:id "supplements"}
                        :transaction-item/credit-account {:id "checking"}}]})

(def ^:private complex-unilateral-trx
  {:id 101
   :transaction/transaction-date (dates/local-date "2020-01-01")
   :transaction/description "Kroger"
   :transaction/entity {:id "personal"}
   :transaction/memo "mid-week necessities"
   :transaction/items [{:transaction-item/quantity (d 120)
                        :transaction-item/account {:id "checking"}
                        :transaction-item/action :credit}
                       {:transaction-item/quantity (d 100)
                        :transaction-item/account {:id "groceries"}
                        :transaction-item/action :debit}
                       {:transaction-item/quantity (d 20)
                        :transaction-item/account {:id "supplements"}
                        :transaction-item/action :debit}]})

(deftest convert-a-transaction-into-a-bilateral
  (testing "a simple transaction"
    (is (= simple-bilateral-trx
           (trx/->bilateral simple-trx))))
  (testing "a simple unilateral transaction"
    (is (= simple-bilateral-trx
           (trx/->bilateral simple-unilateral-trx))))
  (testing "a complex unilateral transaction"
    (is (= complex-bilateral-trx
           (trx/->bilateral complex-unilateral-trx)))))

(deftest convert-a-transaction-into-a-unilateral
  (testing "a simple transaction"
    (is (= simple-unilateral-trx
           (trx/->unilateral simple-trx))))
  (testing "a simple bilateral transaction"
    (is (= simple-unilateral-trx
           (trx/->unilateral simple-bilateral-trx))))
  (testing "a complex bilateral transaction"
    (is (= complex-unilateral-trx
           (trx/->unilateral complex-bilateral-trx)))))

(deftest simplify-a-transaction
  (testing "a bilateral transaction with one item"
    (is (= simple-trx
           (trx/simplify simple-bilateral-trx))))
  (testing "a bilateral transaction with multiple items"
    (is (nil? (trx/simplify complex-bilateral-trx))
        "cannot be created")))
