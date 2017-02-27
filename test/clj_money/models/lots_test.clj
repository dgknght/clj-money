(ns clj-money.models.lots-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.lots :as lots]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private lot-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "IRA"
               :type :asset
               :content-type :commodities}
              {:name "Dining"
               :type :expense}
              {:name "Checking"
               :type :asset
               :content-type :currency}]
   :commodities [{:name "Apple"
                  :symbol "APPL"
                  :exchange :nasdaq}]})

(deftest create-a-lot
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id account)
                                          :purchase-date (t/local-date 2017 3 2)
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (:id result) "The result receives an ID value")
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is (= [{:purchase-date (t/local-date 2017 3 2)
             :shares-owned 100M}] ; shares-owned is set to shares-purchased
           (map #(select-keys % [:purchase-date :shares-owned]) lots))
        "The value is retrieved after create")))

(deftest commodity-id-is-required
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:account-id (:id account)
                                          :purchase-date (t/local-date 2017 3 2)
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :commodity-id))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(deftest account-id-is-required
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:purchase-date (t/local-date 2017 3 2)
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :account-id))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(deftest account-id-must-reference-an-asset-account
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        [_ dining _] (-> context :accounts)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id dining)
                                          :purchase-date (t/local-date 2017 3 2)
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :account-id))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(deftest account-id-must-reference-an-account-with-content-type-commodities
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        [_ _ checking] (-> context :accounts)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id checking)
                                          :purchase-date (t/local-date 2017 3 2)
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :account-id))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(deftest purchase-date-is-required
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id account)
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :purchase-date))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(deftest purchase-date-can-be-a-date-string
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id account)
                                          :purchase-date "2017-03-02" 
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (:id result) "The result receives an ID value")
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is (= [{:purchase-date (t/local-date 2017 3 2)
             :shares-owned 100M}] ; shares-owned is set to shares-purchased
           (map #(select-keys % [:purchase-date :shares-owned]) lots))
        "The value is retrieved after create")))

(deftest purchase-date-must-be-a-date
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id account)
                                          :purchase-date "not-a-date"
                                          :shares-purchased 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :purchase-date))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(deftest shares-purchased-is-required
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        account (-> context :accounts first)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :account-id (:id account)
                                          :purchase-date "2017-03-02"})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id result)) "The result does not receive an ID value")
    (is (not (empty? (validation/error-messages result :shares-purchased))) "The result contains a validation error")
    (is (empty? lots) "The value is not retrieved after create")))

(def ^:private existing-lot-context
  (assoc lot-context :lots [{:account-id "IRA"
                             :commodity-id "APPL"
                             :shares-purchased 100M
                             :shares-owned 100M
                             :purchase-date (t/local-date 2016 3 2)}]))

(deftest update-a-lot
  (let [context (serialization/realize storage-spec existing-lot-context)
        lot (-> context :lots first)
        updated (update-in lot [:shares-owned] #(- % 30M))
        result (lots/update storage-spec updated)
        retrieved (lots/find-by-id storage-spec (:id lot))]
    (is (empty? (validation/error-messages result))
        "The result contains no validation errors")
    (is (= 70M (:shares-owned retrieved))
        "The retrieved map contains the updated value")))
