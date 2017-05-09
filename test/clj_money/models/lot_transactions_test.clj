(ns clj-money.models.lot-transactions-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.lot-transactions :as lot-transactions]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def lot-transaction-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :commodities [{:name "Apple, Inc."
                  :symbol "APPL"
                  :exchange :nasdaq}]
   :accounts [{:name "IRA"
               :type :asset
               :content-type :commodities}
              {:name "Opening balances"
               :type :equity}]
   :lots [{:account-id "IRA"
           :commodity-id "APPL"
           :purchase-date (t/local-date 2016 3 2)
           :shares-owned 100M
           :shares-purchased 100M}]})

(defn- attributes
  [context]
  {:lot-id (-> context :lots first :id)
   :transaction-id 101
   :trade-date (t/local-date 2017 3 2)
   :action :buy
   :shares 100M
   :price 10M})

(deftest create-a-lot-transaction
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (attributes context)
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (->> (lot-transactions/select storage-spec criteria)
                       (map #(dissoc % :id :created-at :updated-at)))]
    (is (empty? (validation/error-messages result)) "The result has no validation errors")
    (is (:id result) "The result contains an ID")
    (is (= [lot-transaction] retrieved)
        "The retrieved value is the same as the stored value.")))

(deftest lot-id-is-required
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (dissoc (attributes context) :lot-id)
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Lot id is required"]
           (validation/error-messages result :lot-id))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(deftest trade-date-is-required
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (dissoc (attributes context) :trade-date)
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Trade date is required"]
           (validation/error-messages result :trade-date))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(deftest trade-date-can-be-a-string-date
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (assoc (attributes context)
                               :trade-date
                               "3/2/2017")
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (->> (lot-transactions/select storage-spec criteria)
                       (map #(dissoc % :id :created-at :updated-at)))
        expected {:trade-date (t/local-date 2017 3 2)
                  :lot-id (-> context :lots first :id)
                  :transaction-id 101
                  :action :buy
                  :shares 100M
                  :price 10M}]
    (is (empty? (validation/error-messages result)) "The result has no validation errors")
    (is (:id result) "The result contains an ID")
    (is (= [expected] retrieved)
        "The retrieved value is the same as the stored value.")))

(deftest trade-date-must-be-a-date
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (-> context
                            attributes
                            (assoc :trade-date "notadate"))
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Trade date must be an instance of class org.joda.time.LocalDate"]
           (validation/error-messages result :trade-date))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(deftest action-is-required
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (-> context
                            attributes
                            (dissoc :action))
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Action is required"]
           (validation/error-messages result :action))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(deftest action-can-be-buy
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (attributes context)
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (->> (lot-transactions/select storage-spec criteria)
                       (map #(dissoc % :id :created-at :updated-at)))]
    (is (empty? (validation/error-messages result)) "The result has no validation errors")
    (is (:id result) "The result contains an ID")
    (is (= [lot-transaction] retrieved)
        "The retrieved value is the same as the stored value.")))

(deftest action-can-be-sell
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (-> context
                            attributes
                            (assoc :action :sell))
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (->> (lot-transactions/select storage-spec criteria)
                       (map #(dissoc % :id :created-at :updated-at)))]
    (is (empty? (validation/error-messages result)) "The result has no validation errors")
    (is (:id result) "The result contains an ID")
    (is (= [lot-transaction] retrieved)
        "The retrieved value is the same as the stored value.")))

(deftest cannot-be-something-other-than-buy-or-sell
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (-> context
                            attributes
                            (assoc :action :not-an-action))
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Action must be one of: sell, buy"]
           (validation/error-messages result :action))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(deftest shares-is-required
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (-> context
                            attributes
                            (dissoc :shares))
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Shares is required"]
           (validation/error-messages result :shares))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(deftest price-is-required
  (let [context (serialization/realize storage-spec lot-transaction-context)
        lot-transaction (-> context
                            attributes
                            (dissoc :price))
        result (lot-transactions/create storage-spec lot-transaction)
        criteria {:lot-id (-> context :lots first :id)}
        retrieved (lot-transactions/select storage-spec criteria)]
    (is (= ["Price is required"]
           (validation/error-messages result :price))
        "The result has a validation error")
    (is (nil? (:id result)) "The result does not contain an ID")
    (is (= [] retrieved) "The value is not retrieved")))

(def ^:private existing-lot-transaction-context
  (assoc lot-transaction-context
         :lot-transactions
         [{:lot-id {:purchase-date (t/local-date 2016 3 2)
                    :account-id "IRA"
                    :commodity-id "APPL"}
           :trade-date (t/local-date 2017 3 2)
           :action :buy
           :shares 100
           :price 10M}]))

(deftest select-lot-transactions-must-have-lot-id
  (let [context (serialization/realize storage-spec
                                       existing-lot-transaction-context)
        criteria {}]
    (is (thrown-with-msg?
          Exception
          #"criteria is not valid"
          (lot-transactions/select storage-spec criteria)))))
