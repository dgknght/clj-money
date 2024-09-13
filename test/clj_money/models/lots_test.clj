(ns clj-money.models.lots-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-account
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.lots :as lots]))

(use-fixtures :each reset-db)

(def ^:private lot-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "IRA"
               :type :asset
               :commodity-id "USD"}
              {:name "Dining"
               :type :expense
               :commodity-id "USD"}
              {:name "Checking"
               :type :asset
               :commodity-id "USD"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}
                 {:name "Apple"
                  :symbol "AAPL"
                  :exchange :nasdaq
                  :type :stock}]})

(defn- attributes
  [context]
  {:commodity-id (:id (find-commodity context "AAPL"))
   :account-id (:id (find-account context "IRA"))
   :purchase-date (t/local-date 2017 3 2)
   :purchase-price 10M
   :shares-purchased 100M})

(deftest create-a-lot
  (let [context (realize lot-context)
        commodity (find-commodity context "AAPL")
        result (lots/create (attributes context))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (:id result) "The result receives an ID value")
    (is (valid? result))
    (is (= [{:purchase-date (t/local-date 2017 3 2)
             :shares-owned 100M
             :purchase-price 10M}] ; shares-owned is set to shares-purchased
           (map #(select-keys % [:purchase-date
                                 :shares-owned
                                 :purchase-price])
                lots))
        "The value is retrieved after create")))

(deftest commodity-id-is-required
  (let [context (realize lot-context)
        commodity (-> context :commodities first)
        result (lots/create (-> context
                                attributes
                                (dissoc :commodity-id)))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:commodity-id] "Commodity is required"))
    (is (empty? lots) "The value is not retrieved after create")))

(deftest account-id-is-required
  (let [context (realize lot-context)
        commodity (-> context :commodities first)
        result (lots/create (-> context
                                attributes
                                (dissoc :account-id)))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:account-id] "Account is required"))
    (is (empty? lots) "The value is not retrieved after create")))

(deftest purchase-price-is-required
  (let [context (realize lot-context)
        commodity (-> context :commodities first)
        result (lots/create (-> context
                                attributes
                                (dissoc :purchase-price)))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:purchase-price] "Purchase price is required"))
    (is (empty? lots) "The value is not retrieved after create")))

(deftest account-id-must-reference-an-asset-account
  (let [context (realize lot-context)
        commodity (-> context :commodities first)
        dining (find-account context "Dining")
        result (lots/create (-> context
                                attributes
                                (assoc :account-id (:id dining))))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:account-id] "Account must be an asset"))
    (is (empty? lots) "The value is not retrieved after create")))

(deftest purchase-date-is-required
  (let [context (realize lot-context)
        commodity (-> context :commodities first)
        result (lots/create (-> context
                                attributes
                                (dissoc :purchase-date)))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:purchase-date] "Purchase date is required"))
    (is (empty? lots) "The value is not retrieved after create")))

(deftest purchase-date-must-be-a-date
  (let [context (realize lot-context)
        commodity (find-commodity context "IRA")
        result (lots/create (-> context
                                attributes
                                (assoc :purchase-date "not-a-date")))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:purchase-date] "Purchase date must be a date"))
    (is (empty? lots) "The value is not retrieved after create")))

(deftest shares-purchased-is-required
  (let [context (realize lot-context)
        commodity (find-commodity context "IRA")
        result (lots/create (-> context
                                attributes
                                (dissoc :shares-purchased)))
        lots (lots/select-by-commodity-id (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (invalid? result [:shares-purchased] "Shares purchased is required"))
    (is (empty? lots) "The value is not retrieved after create")))

(def ^:private existing-lot-context
  (assoc lot-context :lots [{:account-id "IRA"
                             :commodity-id "AAPL"
                             :purchase-price 10M
                             :shares-purchased 100M
                             :shares-owned 100M
                             :purchase-date (t/local-date 2016 3 2)}]))

(deftest update-a-lot
  (let [context (realize existing-lot-context)
        lot (-> context :lots first)
        updated (update-in lot [:shares-owned] #(- % 30M))
        result (lots/update updated)
        retrieved (lots/find lot)]
    (is (valid? result))
    (is (= 70M (:shares-owned retrieved))
        "The retrieved map contains the updated value")))

(deftest search-lots-by-account
  (let [context (realize existing-lot-context)
        ira (find-account context "IRA")
        commodity (->> context
                       :commodities
                       (filter #(= "AAPL" (:symbol %)))
                       first)
        actual (map #(dissoc % :updated-at :created-at :id)
                    (lots/search {:account-id (:id ira)}))
        expected [{:commodity-id (:id commodity)
                   :account-id (:id ira)
                   :purchase-date (t/local-date 2016 3 2)
                   :purchase-price 10M
                   :shares-purchased 100M
                   :shares-owned 100M}]]
    (is (= expected actual) "The correct data is returned")))

; Test unrealized-gains with:
;   Date that precedes some purchases
;   Date that precedes some sales
