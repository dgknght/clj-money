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

(def ^:private reconciliation-context
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

(deftest an-incomplete-reconciliation-can-be-created
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

(deftest a-complete-reconciliation-can-be-created
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

(deftest account-id-is-required
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

(deftest end-of-period-is-required
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

(deftest end-of-period-can-be-an-international-date-string
  (let [context (serialization/realize storage-spec
                                       reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period "2017-01-01"}
        result (reconciliations/create storage-spec reconciliation)
        retrieved (first (reconciliations/find-by-account-id storage-spec (:id checking)))]
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is retrieved "The reconciliation can be retrieved")
    (is (= (t/local-date 2017 1 1) (:end-of-period retrieved)) "The retrieved value should have the correct date")))

(deftest end-of-period-can-be-a-US-date-string
  (let [context (serialization/realize storage-spec
                                       reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period "1/1/2017"}
        result (reconciliations/create storage-spec reconciliation)
        retrieved (first (reconciliations/find-by-account-id storage-spec (:id checking)))]
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is retrieved "The reconciliation can be retrieved")
    (is (= (t/local-date 2017 1 1) (:end-of-period retrieved)) "The retrieved value should have the correct date")))

(deftest end-of-period-cannot-be-a-non-date-string
  (let [context (serialization/realize storage-spec
                                       reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period "notadate"}
        result (reconciliations/create storage-spec reconciliation)]
    (is (= ["End of period must be a date"] (validation/error-messages result :end-of-period)) "The result contains the correct error message")))

(deftest end-of-period-must-come-after-the-previous-end-of-period
  (is false "need to write the test"))

(deftest status-must-be-new-or-completed
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

(deftest balance-must-match-the-calculated-balance
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

(deftest balance-cannot-be-a-non-number-string
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

(deftest item-ids-cannot-be-a-non-number-string
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

(deftest item-ids-must-reference-items-that-belong-to-the-account-being-reconciled
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
                       :balance 500M
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

(deftest a-working-reconciliation-can-be-updated
  (let [context (serialization/realize storage-spec
                                       working-reconciliation-context)
        reconciliation (-> context :reconciliations last)
        updated (assoc reconciliation :balance 1499M)
        result (reconciliations/update storage-spec updated)
        retrieved (reconciliations/find-by-id storage-spec (:id reconciliation))]
    (is (empty? (validation/error-messages result)) "The item has no validation errors")
    (is (= 1499M (:balance retrieved)) "The retrieved value has the correct balance after update")))

(deftest a-working-reconciliation-can-be-completed
  (let [context (serialization/realize storage-spec
                                       working-reconciliation-context)
        checking (-> context :accounts first)
        reconciliation (-> context :reconciliations last)
        updated (assoc reconciliation :status :completed)
        result (reconciliations/update storage-spec updated)
        retrieved (reconciliations/reload storage-spec updated)]
    (is (empty? (validation/error-messages result)) "The funciton completes without validation errors")
    (is (= (:status retrieved) :completed) "The retrieved value has the correct satus")))

(deftest an-out-of-balance-reconciliation-cannot-be-updated-to-completed
  (let [context (serialization/realize storage-spec
                                       working-reconciliation-context)
        checking (-> context :accounts first)
        reconciliation (-> context :reconciliations last)
        updated (-> reconciliation
                    (assoc :status :completed)
                    (update-in [:item-ids] #(conj % (->> context
                                                         :transactions
                                                         (mapcat :items)
                                                         (filter (fn [i] (= (:id checking) (:account-id i))))
                                                         last
                                                         :id))))
        result (reconciliations/update storage-spec updated)]
    (is (validation/has-error? result :balance))))

(deftest a-completed-reconciliation-cannot-be-updated
  (let [context (serialization/realize storage-spec existing-reconciliation-context)
        reconciliation (-> context :reconciliations first)
        updated (assoc reconciliation :balance 1M)
        result (reconciliations/update storage-spec updated)
        retrieved (reconciliations/reload storage-spec reconciliation)]
    (is (validation/has-error? result :status) "A validation error is present")
    (is (= 1000M (:balance retrieved)) "The new valud is not saved")))

; Test that a completed reconciliation can be deleted if it is the most recent
; Test that a completed reconciliation cannot be deleted if it is not the most recent
