(ns clj-money.transactions-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-time.core :as t]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [clj-money.transactions :as trx]))

(def ^:private accounts
  [{:id 1
    :name "Checking"
    :type :asset}
   {:id 2
    :name "Groceries"
    :type :expense}
   {:id 3
    :name "Savings"
    :type :asset}
   {:id 4
    :name "Credit Card"
    :type :liability}])

(def ^:private account-map
  (->> accounts
       (map (juxt (comp ->kebab-case-keyword :name)
                  identity))
       (into {})))

(def ^:private indexed-accounts
  (->> accounts
       (map (juxt :id identity))
       (into {})))

(defn- account-id
  [account-key]
  (get-in account-map [account-key :id]))

(def date (t/local-date 2016 3 2))

(deftest simplify-a-transaction
  (let [transaction {:transaction-date date
                     :description "ACME Store"
                     :memo "transaction memo"
                     :items [{:account-id (account-id :checking)
                              :memo "checking memo" ; NOTE: these memos are lost
                              :action :credit
                              :quantity 10M}
                             {:account-id (account-id :groceries)
                              :memo "groceries memo"
                              :action :debit
                              :quantity 10M}]}
        expected {:transaction-date date
                  :description "ACME Store"
                  :memo "transaction memo"
                  :account-id (account-id :checking)
                  :other-account-id (account-id :groceries)
                  :quantity -10M}]
    (is (= expected (trx/simplify transaction (:checking account-map))))))

(deftest fullify-a-transaction
  (let [expected {:transaction-date date
                  :description "ACME Store"
                  :memo "transaction memo"
                  :items [{:account-id (account-id :checking)
                           :action :credit
                           :quantity 10M}
                          {:account-id (account-id :groceries)
                           :action :debit
                           :quantity 10M}]}
        simple {:transaction-date date
                :description "ACME Store"
                :memo "transaction memo"
                :account-id (account-id :checking)
                :other-account-id (account-id :groceries)
                :quantity -10M}]
    (is (= expected (trx/fullify simple indexed-accounts)))
    (testing "two asset accounts"
      (is (= (assoc-in expected [:items 1 :account-id] (account-id :savings))
             (trx/fullify (assoc simple :other-account-id (account-id :savings))
                          indexed-accounts))))
    (testing "one asset, one liability"
      (is (= (assoc-in expected [:items 1 :account-id] (account-id :credit-card))
             (trx/fullify (assoc simple :other-account-id (account-id :credit-card))
                          indexed-accounts))))))

(deftest entryfy-a-transaction
  (let [transaction {:transaction-date date
                     :description "ACME Store"
                     :memo "transaction memo"
                     :items [{:account-id 1
                              :memo "checking memo"
                              :action :credit
                              :quantity 10M}
                             {:account-id 2
                              :memo "groceries memo"
                              :action :debit
                              :quantity 10M}]}
        expected {:transaction-date date
                  :description "ACME Store"
                  :memo "transaction memo"
                  :items [{:account-id 1
                           :memo "checking memo"
                           :credit-quantity 10M
                           :debit-quantity nil}
                          {:account-id 2
                           :memo "groceries memo"
                           :credit-quantity nil
                           :debit-quantity 10M}
                          {}]}]
    (is (= expected (trx/entryfy transaction)))))

(deftest unentryfy-a-transaction
  (let [expected {:transaction-date date
                  :description "ACME Store"
                  :memo "transaction memo"
                  :items [{:account-id 1
                           :memo "checking memo"
                           :action :credit
                           :quantity 10M
                           :value 10M}
                          {:account-id 2
                           :memo "groceries memo"
                           :action :debit
                           :quantity 10M
                           :value 10M}]}
        transaction {:transaction-date date
                     :description "ACME Store"
                     :memo "transaction memo"
                     :items [{:account-id 1
                              :memo "checking memo"
                              :credit-quantity 10M
                              :debit-quantity nil}
                             {:account-id 2
                              :memo "groceries memo"
                              :credit-quantity nil
                              :debit-quantity 10M}
                             {}]}]
    (is (= expected (trx/unentryfy transaction)))
    (testing "transaction contains 'deleted' items"
      (is (= expected
             (trx/unentryfy (update-in transaction [:items] conj {:account-id 3})))))))

(deftest simplifiability
  (testing "A two-item transaction can be simplified"
    (is (trx/can-simplify? {:items [{:action :debit
                                     :account-id 1
                                     :quantity 10M}
                                    {:action :credit
                                     :account-id 2
                                     :quantity 10M}]})))
  (testing "A transaction with more than two items cannot be simplified"
    (is (not (trx/can-simplify? {:items [{:action :debit
                                          :account-id 1
                                          :quantity 10M}
                                         {:action :credit
                                          :account-id 2
                                          :quantity 6M}
                                         {:action :credit
                                          :account-id 3
                                          :quantity 4M}]})))))

(deftest ensure-an-empty-item
  (let [expected {:items [{:credit-quantity 10M}
                          {:debit-quantity 10M}
                          {}]}]
    (testing "purely empty items"
      (is (= expected
             (trx/ensure-empty-item {:items [{:credit-quantity 10M}
                                             {:debit-quantity 10M}]}))
          "An empty item is added when there are no empty items")
      (is (= expected
             (trx/ensure-empty-item {:items [{:credit-quantity 10M}
                                             {}
                                             {:debit-quantity 10M}
                                             {}]}))
          "An extra empty item is removed")
      (is (= expected
             (trx/ensure-empty-item {:items [{:credit-quantity 10M}
                                             {:debit-quantity 10M}
                                             {}]}))
          "No action is taken if one empty item is present"))))

(def ^:private trading-context
  {:accounts [{:id "401k"
               :name "401k"
               :type :asset
               :tags #{:trading}
               :commodity-id "usd"}
              {:id "aapl"
               :parent-id 1
               :name "AAPL"
               :type :asset
               :tags #{:tradable}
               :commodity-id "aapl"}]
   :commodities [{:id "usd"
                  :name "USD"
                  :symbol "USD"
                  :type :currency}
                 {:id "aapl"
                  :name "Apple, Ince."
                  :symbol "AAPL"
                  :type :stock
                  :exchange :nasdaq}]})

(deftest tradify-an-partial-transaction
  (let [date (t/local-date 2020 3 2)
        tradified (trx/tradify {:transaction-date date
                                :items [{:account-id "401k"
                                         :action :credit}]}
                               {:find-account (->> (:accounts trading-context)
                                                   (map (juxt :id identity))
                                                   (into {}))
                                :find-commodity (->> :commodities trading-context
                                                     (map (juxt :id identity))
                                                     (into {}))})]
    (is (= date (:trade-date tradified)) "The trade-date is taken from transaction-date")
    (is (= "401k" (:account-id tradified)) "The account-id is taken from the item for the trading account")
    (is (= :buy (:action tradified)) "The action defauls to buy")
    (is (nil? (:commodity-id tradified)) "The commodity id is nil")
    (is (nil? (:shares tradified)) "The shares is nil")))

(deftest tradify-a-buy-transaction
  (let [date (t/local-date 2020 3 2)
        accounts (->> (:accounts trading-context)
                      (map (juxt :id identity))
                      (into {}))
        commodities (->> :commodities trading-context
                         (map (juxt :id identity))
                         (into {}))
        standard {:transaction-date date
                  :items [{:account-id "401k"
                           :action :credit
                           :quantity 100M}
                          {:account-id "aapl"
                           :action :debit
                           :quantity 100M}]}
        tradified {:trade-date date
                   :shares 100M
                   :account-id "401k"
                   :commodity-id "aapl"
                   :action :buy}]
    (testing "a standard transaction can be converted to a trade transaction"
      (is (= tradified
             (trx/tradify standard
                          {:find-account accounts
                           :find-commodity commodities}))))
    (testing "a trade transaction can be converted to a standard"
      (is (= standard
             (trx/untradify tradified
                            {:find-account-by-commodity-id (->> (:accounts trading-context)
                                                                (map (juxt :commodity-id identity))
                                                                (into {}))}))))))

(deftest tradify-a-sell-transaction
  (let [date (t/local-date 2020 3 2)
        accounts (->> (:accounts trading-context)
                      (map (juxt :id identity))
                      (into {}))
        commodities (->> :commodities trading-context
                         (map (juxt :id identity))
                         (into {}))
        standard {:transaction-date date
                  :items [{:account-id "aapl"
                           :action :credit
                           :quantity 100M}
                          {:account-id "401k"
                           :action :debit
                           :quantity 100M}]}
        tradified {:trade-date date
                   :shares 100M
                   :account-id "401k"
                   :commodity-id "aapl"
                   :action :sell}]
    (testing "a standard transaction can be converted to a trade transaction"
      (is (= tradified
             (trx/tradify standard
                          {:find-account accounts
                           :find-commodity commodities}))))
    (testing "a trade transaction can be converted to a standard"
      (is (= standard
             (trx/untradify tradified
                            {:find-account-by-commodity-id (->> (:accounts trading-context)
                                                                (map (juxt :commodity-id identity))
                                                                (into {}))}))))))

(deftest summarize-some-items
  (let [items [{:polarized-quantity 100M
                :transaction-date (t/local-date 2016 1 2)}
               {:polarized-quantity 101M
                :transaction-date (t/local-date 2016 1 16)}
               {:polarized-quantity 102M
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
           (trx/summarize-items {:start-date (t/local-date 2016 1 1)
                                       :end-date (t/local-date 2016 4 30)
                                       :interval-type :month
                                       :interval-count 1}
                                items)))))
