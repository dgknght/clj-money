(ns clj-money.transactions-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [clj-money.transactions :as trx]))

(def ^:private accounts
  (->> [{:id :checking
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
         :account/type :liability}]
       (map (juxt :id identity))
       (into {})))

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

; (deftest unentryfy-a-transaction
;   (let [expected {:transaction-date date
;                   :description "ACME Store"
;                   :memo "transaction memo"
;                   :items [{:account {:id 1}
;                            :memo "checking memo"
;                            :action :credit
;                            :quantity 10M
;                            :value 10M}
;                           {:account {:id 2}
;                            :memo "groceries memo"
;                            :action :debit
;                            :quantity 10M
;                            :value 10M}]}
;         transaction {:transaction-date date
;                      :description "ACME Store"
;                      :memo "transaction memo"
;                      :items [{:account {:id 1}
;                               :memo "checking memo"
;                               :credit-quantity 10M
;                               :debit-quantity nil}
;                              {:account {:id 2}
;                               :memo "groceries memo"
;                               :credit-quantity nil
;                               :debit-quantity 10M}
;                              {}]}]
;     (is (= expected (trx/unentryfy transaction)))
;     (testing "transaction contains 'deleted' items"
;       (is (= expected
;              (trx/unentryfy (update-in transaction [:items] conj {:account {:id 3}})))))))
; 
; (deftest simplifiability
;   (testing "A two-item transaction can be simplified"
;     (is (trx/can-simplify? {:items [{:action :debit
;                                      :account {:id 1}
;                                      :quantity 10M}
;                                     {:action :credit
;                                      :account {:id 2}
;                                      :quantity 10M}]})))
;   (testing "A transaction with more than two items cannot be simplified"
;     (is (not (trx/can-simplify? {:items [{:action :debit
;                                           :account {:id 1}
;                                           :quantity 10M}
;                                          {:action :credit
;                                           :account {:id 2}
;                                           :quantity 6M}
;                                          {:action :credit
;                                           :account {:id 3}
;                                           :quantity 4M}]})))))
; 
; (deftest ensure-an-empty-item
;   (let [expected {:items [{:credit-quantity 10M}
;                           {:debit-quantity 10M}
;                           {}]}]
;     (testing "purely empty items"
;       (is (= expected
;              (trx/ensure-empty-item {:items [{:credit-quantity 10M}
;                                              {:debit-quantity 10M}]}))
;           "An empty item is added when there are no empty items")
;       (is (= expected
;              (trx/ensure-empty-item {:items [{:credit-quantity 10M}
;                                              {}
;                                              {:debit-quantity 10M}
;                                              {}]}))
;           "An extra empty item is removed")
;       (is (= expected
;              (trx/ensure-empty-item {:items [{:credit-quantity 10M}
;                                              {:debit-quantity 10M}
;                                              {}]}))
;           "No action is taken if one empty item is present"))))
; 
; (def ^:private trading-context
;   {:accounts [{:id "401k"
;                :name "401k"
;                :type :asset
;                :system-tags #{:trading}
;                :commodity-id "usd"}
;               {:id "aapl"
;                :parent-id 1
;                :name "AAPL"
;                :type :asset
;                :system-tags #{:tradable}
;                :commodity-id "aapl"}]
;    :commodities [{:id "usd"
;                   :name "USD"
;                   :symbol "USD"
;                   :type :currency}
;                  {:id "aapl"
;                   :name "Apple, Ince."
;                   :symbol "AAPL"
;                   :type :stock
;                   :exchange :nasdaq}]})
; 
; (deftest tradify-an-partial-transaction
;   (let [date (t/local-date 2020 3 2)
;         tradified (trx/tradify {:transaction-date date
;                                 :items [{:account-id "401k"
;                                          :action :credit}]}
;                                {:find-account (->> (:accounts trading-context)
;                                                    (map (juxt :id identity))
;                                                    (into {}))
;                                 :find-commodity (->> :commodities trading-context
;                                                      (map (juxt :id identity))
;                                                      (into {}))})]
;     (is (= date (:trade-date tradified)) "The trade-date is taken from transaction-date")
;     (is (= "401k" (:account-id tradified)) "The account-id is taken from the item for the trading account")
;     (is (= :buy (:action tradified)) "The action defauls to buy")
;     (is (nil? (:commodity-id tradified)) "The commodity id is nil")
;     (is (nil? (:shares tradified)) "The shares is nil")))
; 
; (deftest tradify-a-buy-transaction
;   (let [date (t/local-date 2020 3 2)
;         accounts (->> (:accounts trading-context)
;                       (map (juxt :id identity))
;                       (into {}))
;         commodities (->> :commodities trading-context
;                          (map (juxt :id identity))
;                          (into {}))
;         standard {:transaction-date date
;                   :items [{:account-id "401k"
;                            :action :credit
;                            :quantity 100M}
;                           {:account-id "aapl"
;                            :action :debit
;                            :quantity 100M}]}
;         tradified {:trade-date date
;                    :shares 100M
;                    :account-id "401k"
;                    :commodity-id "aapl"
;                    :action :buy}]
;     (testing "a standard transaction can be converted to a trade transaction"
;       (is (= tradified
;              (trx/tradify standard
;                           {:find-account accounts
;                            :find-commodity commodities}))))
;     (testing "a trade transaction can be converted to a standard"
;       (is (= standard
;              (trx/untradify tradified
;                             {:find-account-by-commodity-id (->> (:accounts trading-context)
;                                                                 (map (juxt :commodity-id identity))
;                                                                 (into {}))}))))))
; 
; (deftest tradify-a-sell-transaction
;   (let [date (t/local-date 2020 3 2)
;         accounts (->> (:accounts trading-context)
;                       (map (juxt :id identity))
;                       (into {}))
;         commodities (->> :commodities trading-context
;                          (map (juxt :id identity))
;                          (into {}))
;         standard {:transaction-date date
;                   :items [{:account-id "aapl"
;                            :action :credit
;                            :quantity 100M}
;                           {:account-id "401k"
;                            :action :debit
;                            :quantity 100M}]}
;         tradified {:trade-date date
;                    :shares 100M
;                    :account-id "401k"
;                    :commodity-id "aapl"
;                    :action :sell}]
;     (testing "a standard transaction can be converted to a trade transaction"
;       (is (= tradified
;              (trx/tradify standard
;                           {:find-account accounts
;                            :find-commodity commodities}))))
;     (testing "a trade transaction can be converted to a standard"
;       (is (= standard
;              (trx/untradify tradified
;                             {:find-account-by-commodity-id (->> (:accounts trading-context)
;                                                                 (map (juxt :commodity-id identity))
;                                                                 (into {}))}))))))
; 
; (deftest summarize-some-items
;   (let [items [{:polarized-quantity 100M
;                 :transaction-date (t/local-date 2016 1 2)}
;                {:polarized-quantity 101M
;                 :transaction-date (t/local-date 2016 1 16)}
;                {:polarized-quantity 102M
;                 :transaction-date (t/local-date 2016 3 1)}]
;         expected [{:start-date (t/local-date 2016 1 1)
;                    :end-date (t/local-date 2016 1 31)
;                    :quantity 201M}
;                   {:start-date (t/local-date 2016 2 1)
;                    :end-date (t/local-date 2016 2 29)
;                    :quantity 0M}
;                   {:start-date (t/local-date 2016 3 1)
;                    :end-date (t/local-date 2016 3 31)
;                    :quantity 102M}
;                   {:start-date (t/local-date 2016 4 1)
;                    :end-date (t/local-date 2016 4 30)
;                    :quantity 0M}]]
;     (is (= expected
;            (trx/summarize-items {:start-date (t/local-date 2016 1 1)
;                                        :end-date (t/local-date 2016 4 30)
;                                        :interval-type :month
;                                        :interval-count 1}
;                                 items)))))
; 
; (deftest expand-a-transaction
;   (let [expected {:transaction-date date
;                   :description "ACME Store"
;                   :memo "transaction memo"
;                   :items [{:account-id (account-id :groceries)
;                            :action :debit
;                            :quantity 10M}
;                           {:account-id (account-id :checking)
;                            :action :credit
;                            :quantity 10M}]}
;         simple {:transaction-date date
;                 :description "ACME Store"
;                 :memo "transaction memo"
;                 :debit-account-id (account-id :groceries)
;                 :credit-account-id (account-id :checking)
;                 :quantity 10M}]
;     (is (= expected (trx/expand simple)))))
; 
; (deftest calc-the-value-of-a-transaction
;   (is (= 100M (trx/value #:transaction{:items [#:transaction-item{:quantity 100M
;                                                                   :value 100M
;                                                                   :action :credit
;                                                                   :account {:id :checking}}
;                                                #:transaction-item{:quantity 100M
;                                                                   :value 100M
;                                                                   :action :debit
;                                                                   :account {:id :groceries}}]}))
;       "The value is the sum of credits (or debits)")
;   (is (nil? (trx/value #:transaction{:items [#:transaction-item{:quantity 101M
;                                                                 :value 101M
;                                                                 :action :credit
;                                                                 :account {:id :checking}}
;                                              #:transaction-item{:quantity 100M
;                                                                 :value 100M
;                                                                 :action :debit
;                                                                 :account {:id :groceries}}]}))
;       "The value is nil (undeterminable) if the credits and debits do not match"))
