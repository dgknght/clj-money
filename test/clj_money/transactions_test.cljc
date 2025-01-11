(ns clj-money.transactions-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.util :as util]
            [clj-money.transactions :as trx]))

(defn- index
  [& inputs]
  (let [[->key & models] (if (map? (first inputs))
                           (cons util/->model-ref inputs)
                           inputs)]
    (->> models
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
                                   :transaction-item/quantity 10M}
                                  {:id 2
                                   :transaction-item/account (accounts :groceries)
                                   :transaction-item/memo "groceries memo"
                                   :transaction-item/action :debit
                                   :transaction-item/quantity 10M}]}
        expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :item {:id 1 }
                               :account (accounts :checking)
                               :other-item {:id 2} :other-account (accounts :groceries)
                               :quantity -10M}]
    (is (= expected (trx/accountify trx (accounts :checking))))))

(deftest unaccountify-a-transaction
  (let [expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [{:id 1
                                        :transaction-item/account {:id :checking}
                                        :transaction-item/action :credit
                                        :transaction-item/quantity 10M}
                                       {:id 2
                                        :transaction-item/account {:id :groceries}
                                        :transaction-item/action :debit
                                        :transaction-item/quantity 10M}]}
        simple #:transaction{:transaction-date "2020-01-01"
                             :description "ACME Store"
                             :memo "transaction memo"
                             :item {:id 1}
                             :account {:id :checking}
                             :other-item {:id 2}
                             :other-account {:id :groceries}
                             :quantity -10M}]
    (is (= expected (trx/unaccountify simple (comp accounts :id))))
    (testing "two asset accounts"
      (is (= (assoc-in expected [:transaction/items 1 :transaction-item/account] {:id :savings})
             (trx/unaccountify (assoc simple :transaction/other-account {:id :savings})
                               (comp accounts :id)))))
    (testing "one asset, one liability"
      (is (= (assoc-in expected [:transaction/items 1 :transaction-item/account] {:id :credit-card})
             (trx/unaccountify (assoc simple :transaction/other-account {:id :credit-card})
                               (comp accounts :id)))))))

(deftest entryfy-a-transaction
  (let [transaction #:transaction{:transaction-date "2020-01-01"
                                  :description "ACME Store"
                                  :memo "transaction memo"
                                  :items [#:transaction-item{:account {:id 1}
                                                             :memo "checking memo"
                                                             :action :credit
                                                             :quantity 10M}
                                          #:transaction-item{:account {:id 2}
                                                             :memo "groceries memo"
                                                             :action :debit
                                                             :quantity 10M}]}
        expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [#:transaction-item{:account {:id 1}
                                                          :memo "checking memo"
                                                          :credit-quantity 10M
                                                          :debit-quantity nil}
                                       #:transaction-item{:account {:id 2}
                                                          :memo "groceries memo"
                                                          :credit-quantity nil
                                                          :debit-quantity 10M}
                                       {}]}]
    (is (= expected (trx/entryfy transaction)))))

(deftest unentryfy-a-transaction
  (let [expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [#:transaction-item{:account {:id 1}
                                                          :memo "checking memo"
                                                          :action :credit
                                                          :quantity 10M
                                                          :value 10M}
                                       #:transaction-item{:account {:id 2}
                                                          :memo "groceries memo"
                                                          :action :debit
                                                          :quantity 10M
                                                          :value 10M}]}
        transaction #:transaction{:transaction-date "2020-01-01"
                                  :description "ACME Store"
                                  :memo "transaction memo"
                                  :items [#:transaction-item{:account {:id 1}
                                                             :memo "checking memo"
                                                             :credit-quantity 10M
                                                             :debit-quantity nil}
                                          #:transaction-item{:account {:id 2}
                                                             :memo "groceries memo"
                                                             :credit-quantity nil
                                                             :debit-quantity 10M}
                                          {}]}]
    (is (= expected (trx/unentryfy transaction)))
    (testing "transaction contains 'deleted' items"
      (is (= expected
             (trx/unentryfy (update-in transaction
                                       [:transaction/items]
                                       conj
                                       {:transaction-item/account {:id 3}})))))))

(deftest simplifiability
  (testing "A two-item transaction can be simplified"
    (is (trx/can-simplify?
          {:transaction/items [#:transaction-item{:action :debit
                                                  :account {:id 1}
                                                  :quantity 10M}
                               #:transaction-item{:action :credit
                                                  :account {:id 2}
                                                  :quantity 10M}]})))
  (testing "A transaction with more than two items cannot be simplified"
    (is (not (trx/can-simplify?
               {:transaction/items [#:transaction-item{:action :debit
                                                       :account {:id 1}
                                                       :quantity 10M}
                                    #:transaction-item{:action :credit
                                                       :account {:id 2}
                                                       :quantity 6M}
                                    #:transaction-item{:action :credit
                                                       :account {:id 3}
                                                       :quantity 4M}]})))))

(deftest ensure-an-empty-item
  (let [expected {:transaction/items [#:transaction-item{:credit-quantity 10M}
                                      #:transaction-item{:debit-quantity 10M}
                                      {}]}]
    (testing "purely empty items"
      (is (= expected
             (trx/ensure-empty-item {:transaction/items [#:transaction-item{:credit-quantity 10M}
                                                         #:transaction-item{:debit-quantity 10M}]}))
          "An empty item is added when there are no empty items")
      (is (= expected
             (trx/ensure-empty-item {:transaction/items [#:transaction-item{:credit-quantity 10M}
                                                         {}
                                                         #:transaction-item{:debit-quantity 10M}
                                                         {}]}))
          "An extra empty item is removed")
      (is (= expected
             (trx/ensure-empty-item {:transaction/items [#:transaction-item{:credit-quantity 10M}
                                                         #:transaction-item{:debit-quantity 10M}
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
    (is (util/model= {:id :401k} (:trade/account tradified)) "The account is taken from the item for the trading account")
    (is (= :buy (:trade/action tradified)) "The action defauls to buy")
    (is (nil? (:trade/commodity tradified)) "The commodity id is nil")
    (is (nil? (:trade/shares tradified)) "The shares is nil")))

(deftest tradify-a-buy-transaction
  (let [standard #:transaction{:transaction-date "2020-01-01"
                               :items [#:transaction-item{:account {:id :401k}
                                                          :action :credit
                                                          :quantity 100M}
                                       #:transaction-item{:account {:id :aapl}
                                                          :action :debit
                                                          :quantity 100M}]}
        tradified #:trade{:trade-date "2020-01-01"
                          :shares 100M
                          :account {:id :401k}
                          :commodity {:id :aapl}
                          :action :buy}]
    (testing "a standard transaction can be converted to a trade transaction"
      (is (= tradified
             (-> (trx/tradify standard
                              {:find-account (:accounts trading-context)
                               :find-commodity (:commodities trading-context)})
                 (update-in [:trade/account] util/->model-ref)
                 (update-in [:trade/commodity] util/->model-ref)))))
    (testing "a trade transaction can be converted to a standard"
      (is (= standard
             (update-in (trx/untradify tradified
                            {:find-account-with-commodity find-account-with-commodity})
                        [:transaction/items]
                        (fn [items]
                          (map #(update-in %
                                           [:transaction-item/account]
                                           util/->model-ref)
                               items))))))))

(deftest tradify-a-sell-transaction
  (let [standard #:transaction{:transaction-date "2020-01-01"
                               :items [#:transaction-item{:account {:id :aapl}
                                                          :action :credit
                                                          :quantity 100M}
                                       #:transaction-item{:account {:id :401k}
                                                          :action :debit
                                                          :quantity 100M}]}
        tradified #:trade{:trade-date "2020-01-01"
                          :shares 100M
                          :account {:id :401k}
                          :commodity {:id :aapl}
                          :action :sell}]
    (testing "a standard transaction can be converted to a trade transaction"
      (is (= tradified
             (-> (trx/tradify standard
                              {:find-account (:accounts trading-context)
                               :find-commodity (:commodities trading-context)})
                 (update-in [:trade/account] util/->model-ref)
                 (update-in [:trade/commodity] util/->model-ref)))))
    (testing "a trade transaction can be converted to a standard"
      (is (= standard
             (update-in (trx/untradify tradified
                            {:find-account-with-commodity find-account-with-commodity})
                        [:transaction/items]
                        (fn [items]
                          (mapv #(update-in % [:transaction-item/account] util/->model-ref)
                                items))))))))

(deftest summarize-some-items
  (let [items [#:transaction-item{:polarized-quantity 100M
                                  :transaction-date (t/local-date 2016 1 2)}
               #:transaction-item{:polarized-quantity 101M
                                  :transaction-date (t/local-date 2016 1 16)}
               #:transaction-item{:polarized-quantity 102M
                                  :transaction-date (t/local-date 2016 3 1)}]
        expected [{:start-date (t/local-date 2016 1 1)
                   :end-date (t/local-date 2016 1 31)
                   :quantity 201M}
                  {:start-date (t/local-date 2016 2 1)
                   :end-date (t/local-date 2016 2 29)
                   :quantity 0M}
                  {:start-date (t/local-date 2016 3 1)
                   :end-date (t/local-date 2016 3 31)
                   :quantity 102M}
                  {:start-date (t/local-date 2016 4 1)
                   :end-date (t/local-date 2016 4 30)
                   :quantity 0M}]]
    (is (= expected
           (trx/summarize-items {:since (t/local-date 2016 1 1)
                                 :as-of (t/local-date 2016 4 30)
                                 :interval-type :month
                                 :interval-count 1}
                                items)))))

(deftest expand-a-transaction
  (let [expected #:transaction{:transaction-date "2020-01-01"
                               :description "ACME Store"
                               :memo "transaction memo"
                               :items [#:transaction-item{:account {:id :groceries}
                                                          :action :debit
                                                          :quantity 10M}
                                       #:transaction-item{:account {:id :checking}
                                                          :action :credit
                                                          :quantity 10M}]}
        simple #:transaction{:transaction-date "2020-01-01"
                             :description "ACME Store"
                             :memo "transaction memo"
                             :debit-account {:id :groceries}
                             :credit-account {:id :checking}
                             :quantity 10M}]
    (is (= expected (trx/expand simple))
        "A simple transaction is expanded")
    (is (= expected (trx/expand expected))
        "An expanded transaction is returned as-is")))

(deftest calc-the-value-of-a-transaction
  (is (= 100M (trx/value #:transaction{:items [#:transaction-item{:quantity 100M
                                                                  :value 100M
                                                                  :action :credit
                                                                  :account {:id :checking}}
                                               #:transaction-item{:quantity 100M
                                                                  :value 100M
                                                                  :action :debit
                                                                  :account {:id :groceries}}]}))
      "The value is the sum of credits (or debits)")
  (is (nil? (trx/value #:transaction{:items [#:transaction-item{:quantity 101M
                                                                :value 101M
                                                                :action :credit
                                                                :account {:id :checking}}
                                             #:transaction-item{:quantity 100M
                                                                :value 100M
                                                                :action :debit
                                                                :account {:id :groceries}}]}))
      "The value is nil (undeterminable) if the credits and debits do not match"))
