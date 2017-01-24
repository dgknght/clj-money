(ns clj-money.models.reconciliations-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.util :refer [pprint-and-return]]
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

(deftest create-an-incomplete-reconciliation
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
                        :end-of-period (t/local-date 2017 1 31)}
        result (reconciliations/create storage-spec reconciliation)
        retrieved (first (reconciliations/find-by-account-id storage-spec (:id checking)))]
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is retrieved "The reconciliation can be retrieved")
    (testing "transaction items are not marked as reconciled"
      (is (= #{false}
             (->> context
                  :transactions
                  (map #(transactions/reload storage-spec %))
                  (mapcat :items)
                  (filter #(= (:id checking) (:account-id %)))
                  (map :reconciled?)
                  set))
          "None of the transaction items should be marked as reconcilied"))))

(deftest create-a-complete-reconciliation
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
                        :end-of-period (t/local-date 2017 1 31)
                        :item-ids (map :id [paycheck landlord safeway])
                        :status :completed}
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

(deftest attempt-to-create-a-reconciliation-without-an-account-id
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
        result (reconciliations/create storage-spec
                                       {:balance 447M
                                        :item-ids (map :id [paycheck landlord safeway])})]
    (is (validation/has-error? result :account-id))))

(deftest attempt-to-create-a-reconciliation-without-an-end-of-period
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
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :balance 447M
                                        :item-ids (map :id [paycheck landlord safeway])})]
    (is (validation/has-error? result :end-of-period))))

(deftest attempt-to-create-a-reconciliation-with-and-invalid-status
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
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :status :bouncy
                                        :balance 447M
                                        :item-ids (map :id [paycheck landlord safeway])})]
    (is (validation/has-error? result :status))))

(deftest attempt-to-create-a-reconciliation-without-a-balance
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
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)})]
    (is (validation/has-error? result :balance))))

(deftest attempt-to-create-a-reconciliation-with-an-invalid-balance
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
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :balance "notanumber"})]
    (is (validation/has-error? result :balance))))

(deftest attempt-to-create-a-reconciliation-with-invalid-item-ids
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
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :balance 10M
                                        :item-ids "notvalid"})]
    (is (validation/has-error? result :item-ids))))

(deftest attempt-to-create-a-reconciliation-with-transaction-items-from-another-account
  (let [context (serialization/realize storage-spec
                                       reconciliation-context)
        [checking
         salary] (:accounts context)
        paycheck (->> context
                      :transactions
                      (mapcat :items)
                      (filter #(= (:id salary) (:account-id %)))
                      first)
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :balance 10M
                                        :item-ids [(:id paycheck)]})]
    (is (validation/has-error? result :item-ids))))

(deftest attempt-to-create-a-reconciliation-that-is-not-balanced
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
        result (reconciliations/create storage-spec
                                       {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :status :completed
                                        :balance 10M
                                        :item-ids (map :id [paycheck landlord safeway])})]
    (is (validation/has-error? result :balance))))

(def ^:private working-rec-context
  (assoc reconciliation-context
         :reconciliations
         [{:account-id "Checking"
           :end-of-period (t/local-date 2017 1 1)
           :balance 447M
           :status :new
           :item-ids [{:transaction-date (t/local-date 2017 1 1)
                       :account-id "Salary"
                       :amount 1000M}
                      {:transaction-date (t/local-date 2017 1 2)
                       :account-id "Rent"
                       :amount 500M}
                      {:transaction-date (t/local-date 2017 1 10)
                       :account-id "Groceries"
                       :amount 53M}]}]))

(deftest find-the-working-reconciliation
  (let [context (serialization/realize storage-spec
                                       working-rec-context)
        checking (-> context :accounts first)
        reconciliation (reconciliations/find-working storage-spec (:id checking))]
    (is reconciliation "A reconciliation is returned")))

(def ^:private existing-reconciliation-context
  (assoc reconciliation-context
         :reconciliations
         [{:account-id "Checking"
           :end-of-period (t/local-date 2017 1 1)
           :balance 1000M
           :status :completed
           :item-ids [{:transaction-date (t/local-date 2017 1 1)
                       :amount 1000M}]}]))

(def ^:private working-reconciliation-context
  (update-in existing-reconciliation-context
             [:reconciliations]
             #(conj % {:account-id "Checking"
                       :end-of-period (t/local-date 2017 1 2)
                       :balance 1500M
                       :status :new
                       :item-ids [{:transaction-date (t/local-date 2017 1 2)
                                   :amount 500M}]})))

(deftest find-the-previous-reconciled-balance
  (let [context (serialization/realize storage-spec working-reconciliation-context)
        checking (-> context :accounts first)]
    (is (= 1000M (reconciliations/previous-balance storage-spec (:id checking)))
        "The previous balance is the balance of the last completed reconciliation")))

(deftest transaction-item-can-only-belong-to-one-reconciliation
  (let [context (serialization/realize storage-spec
                                       existing-reconciliation-context)
        checking (-> context :accounts first)
        item (->> (:transactions context)
                  (mapcat :items)
                  (filter #(= (:id checking) (:account-id %)))
                  first)
        result (reconciliations/create storage-spec {:account-id (:id checking)
                                                     :end-of-period (t/local-date 2017 1 31)
                                                     :balance 1500M
                                                     :item-ids [(:id item)]})]
    (is (validation/has-error? result :item-ids)
        "An item ID that is already reconconciled should be invalid")))
