(ns clj-money.web.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.serialization :as serialization]
            [clj-factory.core :refer [factory]]
            [clj-money.validation :as validation]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.transactions :as transm]
            [clj-money.web.transactions :as transactions]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def base-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Bonus"
               :type :income}]})

(def create-context
  base-context)

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

(def update-context
  (merge base-context
         {:transactions [{:transaction-date (t/local-date 2016 1 1)
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :amount (bigdec 1000)}
                                  {:action :credit
                                   :account-id "Salary"
                                   :amount (bigdec 1000)}]}]}))

(deftest update-a-transaction
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         bonus] (:accounts context)
        [trans] (:transactions context)
        _ (transactions/update {:id (str (:id trans))
                                :transaction-date "2016-01-02"
                                :description "Employer"
                                :id-0 (-> trans :items first :id str)
                                :account-id-0 (str (:id checking))
                                :credit-amount-0 ""
                                :debit-amount-0 "1001"
                                :id-1 (-> trans :items second :id str)
                                :account-id-1 (str (:id salary))
                                :credit-amount-1 "901"
                                :debit-amount-1 ""
                                :id-2 ""
                                :account-id-2 (str (:id bonus))
                                :credit-amount-2 "100"
                                :debit-amount-2 ""
                                :id-3 ""
                                :account-id-3 ""
                                :credit-amount-3 ""
                                :debit-amount-3 ""})
        actual (simplify-transaction (transm/find-by-id storage-spec (:id trans)))
        expected {:transaction-date (t/local-date 2016 1 2)
            :description "Employer"
            :entity-id (-> context :entities first :id)
            :items [{:action :credit
                     :account-id (:id bonus)
                     :amount (bigdec 100)
                     :balance (bigdec 100)
                     :index 0}
                    {:action :credit
                     :account-id (:id salary)
                     :amount (bigdec 901)
                     :balance (bigdec 901)
                     :index 0}
                    {:action :debit
                     :account-id (:id checking)
                     :amount (bigdec 1001)
                     :balance (bigdec 1001)
                     :index 0}]}]
    (is (= expected actual) "The updated transaction can be retrieved")))
