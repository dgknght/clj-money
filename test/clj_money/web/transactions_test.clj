(ns clj-money.web.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.serialization :as serialization]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.transactions :as transm]
            [clj-money.web.transactions :as transactions]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def create-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}]})

(defn- simplify-transaction-item
  [item]
  (dissoc item :id
          :transaction-id
          :updated-at
          :created-at))

(defn- simplify-transaction
  [transaction]
  (-> transaction
      (dissoc :id :created-at :updated-at)
      (update-in [:items] #(map simplify-transaction-item %))))

(deftest create-a-transaction
  (let [context (serialization/realize storage-spec create-context)
        [checking
         salary] (:accounts context)
        entity-id (-> context :entities first :id)
        _ (transactions/create {:entity-id entity-id
                                :transaction-date "3/2/2016"
                                :description "Paycheck"
                                :account-id-0 (str (:id checking))
                                :debit-amount-0 "1000"
                                :credit-amount-0 ""
                                :account-id-1 (str (:id salary))
                                :debit-amount-1 ""
                                :credit-amount-1 "1000"})
        actual (map simplify-transaction
                    (transm/select-by-entity-id storage-spec entity-id))
        expected [{:transaction-date (t/local-date 2016 3 2)
                   :description "Paycheck"
                   :entity-id entity-id
                   :items [{:account-id (:id salary)
                            :amount (bigdec 1000)
                            :action :credit
                            :balance (bigdec 1000)
                            :index 0}
                           {:account-id (:id checking)
                            :amount (bigdec 1000)
                            :action :debit
                            :balance (bigdec 1000)
                            :index 0}]}]]
    (is (= expected actual) "The transaction should be retrievable")))
