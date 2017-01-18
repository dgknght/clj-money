(ns clj-money.models.reconciliations-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.models.reconciliations :as reconciliations]
            [clj-money.models.transactions :as transactions]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def reconciliation-context
  {:users [{:email "john@doe.com"
            :first-name "John"
            :last-name "Doe"
            :password "please01"
            :password-confirmation "please01"}]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Rent"
               :type :expense}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2017 1 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000M}]}
                  {:transaction-date (t/local-date 2017 1 2)
                   :description "Landlord"
                   :items [{:action :debit
                            :account-id "Rent"
                            :amount 500M}
                           {:action :credit
                            :account-id "Checking"
                            :amount 500M}]}
                  {:transaction-date (t/local-date 2017 1 3)
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 45M}
                           {:action :credit
                            :account-id "Checking"
                            :amount 45M}]}
                  {:transaction-date (t/local-date 2017 1 10)
                   :description "Safeway"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 53M}
                           {:action :credit
                            :account-id "Checking"
                            :amount 53M}]}]})

(deftest reconcile-an-account
  (let [context (serialization/realize storage-spec
                                       reconciliation-context)
        checking (-> context :accounts first)
        [paycheck
         landlord
         kroger
         safeway] (->> context
                       :transactions
                       (mapcat :items)
                       (filter #(= (:id checking) (:account-id %))))
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :item-ids (map :id [paycheck landlord safeway])}
        result (reconciliations/create storage-spec reconciliation)
        retrieved (first (reconciliations/find-by-account-id storage-spec (:id checking)))]
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is retrieved "The reconciliation can be retrieved")
    (testing "transaction items are marked as reconciled"
      (is (= [true true false true]
             (->> context
                  :transactions
                  (map #(transactions/reload storage-spec %))
                  (mapcat :items)
                  (filter #(= (:id checking) (:account-id %)))
                  (map :reconciled?)))
          "Each transaction item included in the reconciliation should be marked as reconciled"))))
