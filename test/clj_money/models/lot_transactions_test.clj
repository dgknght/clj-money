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
               :type :equity}]})

(deftest create-a-lot-transaction
  (let [context (serialization/realize storage-spec lot-transaction-context)
        [ira] (:accounts context)
        [commodity] (:commodities context)
        lot-transaction {:account-id (:id ira)
                         :commodity-id (:id commodity)
                         :trade-date (t/local-date 2017 3 2)
                         :action :buy
                         :shares 100M
                         :price 10M}
        result (lot-transactions/create storage-spec lot-transaction)
        retrieved (->> (lot-transactions/select storage-spec
                                                {:account-id (:id ira)
                                                 :commodity-id (:id commodity)})
                       (map #(dissoc % :id :created-at :updated-at)))]
    (is (empty? (validation/error-messages result)) "The result has no validation errors")
    (is (:id result) "The result contains an ID")
    (is (= [lot-transaction] retrieved)
        "The retrieved value is the same as the stored value.")))

(deftest account-id-is-required
  (is false "need to write the test"))

(deftest commodity-id-is-required
  (is false "need to write the test"))

(deftest trade-date-is-required
  (is false "need to write the test"))

(deftest trade-date-can-be-a-string-date
  (is false "need to write the test"))

(deftest trade-date-can-must-be-a-date
  (is false "need to write the test"))

(deftest action-is-required
  (is false "need to write the test"))

(deftest action-can-be-buy
  (is false "need to write the test"))

(deftest action-can-be-sell
  (is false "need to write the test"))

(deftest cannot-be-something-other-than-buy-or-sell
  (is false "need to write the test"))

(deftest shares-is-required
  (is false "need to write the test"))

(deftest price-is-required
  (is false "need to write the test"))

(deftest select-lot-transactions-must-have-commodity-id
  (is false "need to write the test"))

(deftest select-lot-transactions-must-have-account-id
  (is false "need to write the test"))
