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
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
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

(def ^:private existing-reconciliation-context
  (assoc reconciliation-context
         :reconciliations
         [{:account-id "Checking"
           :end-of-period (t/local-date 2017 1 1)
           :balance 1000M
           :status :completed
           :item-refs [{:transaction-date (t/local-date 2017 1 1)
                       :amount 1000M}]}]))

(def ^:private working-reconciliation-context
  (update-in existing-reconciliation-context
             [:reconciliations]
             #(conj % {:account-id "Checking"
                       :end-of-period (t/local-date 2017 1 2)
                       :balance 500M
                       :status :new
                       :item-refs [{:transaction-date (t/local-date 2017 1 2)
                                   :amount 500M}]})))

(deftest create-a-reconciliation
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
        retrieved (reconciliations/find storage-spec {:account-id (:id checking)})]
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

(deftest create-a-completed-reconciliation
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
                        :item-refs (map (juxt :id :transaction-date) [paycheck landlord safeway])
                        :status :completed}
        result (reconciliations/create storage-spec reconciliation)
        retrieved (reconciliations/find storage-spec {:account-id (:id checking)})]
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

(deftest a-new-reconciliation-cannot-be-created-if-one-already-exists
  (let [context (serialization/realize storage-spec working-reconciliation-context)
        checking (-> context :accounts first)
        result (reconciliations/create storage-spec {:account-id (:id checking)
                                                     :balance 1M
                                                     :status :new
                                                     :end-of-period (t/local-date 2017 2 28)})]
    (is (not (validation/valid? result)) "The result is not valid")
    (is (= ["A new reconciliation cannot be created while a working reconciliation already exists"]
           (validation/error-messages result :account-id)))))

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
                                        :item-refs (map :id [paycheck landlord safeway])})]
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
                                        :item-refs (map :id [paycheck landlord safeway])})]
    (is (validation/has-error? result :end-of-period))))

(deftest end-of-period-can-be-an-international-date-string
  (let [context (serialization/realize storage-spec
                                       reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period "2017-01-01"}
        result (reconciliations/create storage-spec reconciliation)
        retrieved (reconciliations/find storage-spec {:account-id (:id checking)})]
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
        retrieved (reconciliations/find storage-spec {:account-id (:id checking)})]
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
  (let [context (serialization/realize storage-spec
                                       existing-reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period (t/local-date 2016 12 31)}
        result (reconciliations/create storage-spec reconciliation)]
    (is (= ["End of period must be after the latest reconciliation"]
           (validation/error-messages result :end-of-period))
        "The result contains the correct error message")))

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
                                        :item-refs (map :id [paycheck landlord safeway])})]
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

(deftest item-refs-cannot-be-a-non-number-string
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
                                        :item-refs "notvalid"})]
    (is (validation/has-error? result :item-refs))))

(deftest item-refs-must-reference-items-that-belong-to-the-account-being-reconciled
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
                                        :item-refs [((juxt :id :transaction-date) paycheck)]})]
    (is (validation/has-error? result :item-refs))))

(def ^:private working-rec-context
  (assoc reconciliation-context
         :reconciliations
         [{:account-id "Checking"
           :end-of-period (t/local-date 2017 1 1)
           :balance 447M
           :status :new
           :item-refs [{:transaction-date (t/local-date 2017 1 1)
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

(deftest find-the-last-completed-reconciliation
  (let [context (serialization/realize storage-spec working-reconciliation-context)
        checking (-> context :accounts first)]
    (is (= 1000M (:balance (reconciliations/find-last-completed storage-spec (:id checking))))
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
                                                     :item-refs [((juxt :id :transaction-date) item)]})]
    (is (validation/has-error? result :item-refs)
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
                    (update-in [:item-refs] #(conj % (->> context
                                                         :transactions
                                                         (mapcat :items)
                                                         (filter (fn [i] (= (:id checking) (:account-id i))))
                                                         (map (juxt :id :transaction-date))
                                                         last))))
        result (reconciliations/update storage-spec updated)]
    (is (validation/has-error? result :balance))))

(deftest the-amount-and-action-of-a-reconciled-item-cannot-be-changed
  (let [context (serialization/realize storage-spec existing-reconciliation-context)
        reconciliation (-> context :reconciliations first)
        item (first (transactions/select-items-by-reconciliation storage-spec reconciliation))
        transaction (transactions/find-by-id storage-spec
                                             (:transaction-id item)
                                             (:transaction-date item))
        result1 (transactions/update storage-spec (update-in transaction [:items]
                                                             #(map (fn [item]
                                                                     (assoc item :amount 1M))
                                                                   %)))
        result2 (transactions/update storage-spec (update-in transaction [:items]
                                                             #(map (fn [item]
                                                                     (update-in item [:action] (fn [a] (if (= :credit a)
                                                                                                         :debit
                                                                                                         :credit))))
                                                                   %)))]
    (is (= ["A reconciled transaction item cannot be changed"]
           (validation/error-messages result1 :items)))
    (is (= ["A reconciled transaction item cannot be changed"]
           (validation/error-messages result2 :items)))))

(deftest a-reconciled-transaction-item-cannot-be-deleted
  (let [context (serialization/realize storage-spec existing-reconciliation-context)
        [item-id date] (-> context :reconciliations first :item-refs first)
        transaction-id (:id (transactions/find-by-item-id storage-spec item-id date))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"A transaction with reconciled items cannot be deleted."
                            (transactions/delete storage-spec transaction-id date))
          "An exception is raised")
    (is (do
          (try
            (transactions/delete storage-spec transaction-id date)
            (catch clojure.lang.ExceptionInfo e nil))
          (transactions/find-by-item-id storage-spec item-id date))
        "The transaction can be retrieved after the delete has been denied")))

(deftest a-completed-reconciliation-cannot-be-updated
  (let [context (serialization/realize storage-spec existing-reconciliation-context)
        reconciliation (-> context :reconciliations first)
        updated (assoc reconciliation :balance 1M)
        result (reconciliations/update storage-spec updated)
        retrieved (reconciliations/reload storage-spec reconciliation)]
    (is (validation/has-error? result :status) "A validation error is present")
    (is (= 1000M (:balance retrieved)) "The new valud is not saved")))

(deftest the-most-recent-completed-reconciliation-can-be-deleted
  (let [context (serialization/realize storage-spec existing-reconciliation-context)
        reconciliation (-> context :reconciliations first)
        _ (reconciliations/delete storage-spec (:id reconciliation))
        retrieved (reconciliations/find-by-id storage-spec (:id reconciliation))
        items (transactions/select-items-by-reconciliation storage-spec reconciliation)]
    (is (nil? retrieved) "The reconciliation cannot be retrieved after delete")
    (is (empty? items) "The reconciliation is not associated with any items after delete")))

(deftest a-working-reconciliation-can-be-deleted
  (let [context (serialization/realize storage-spec working-reconciliation-context)
        reconciliation (-> context :reconciliations second)
        _ (reconciliations/delete storage-spec (:id reconciliation))
        retrieved (reconciliations/find-by-id storage-spec (:id reconciliation))
        items (transactions/select-items-by-reconciliation storage-spec reconciliation)]
    (is (nil? retrieved) "The reconciliation cannot be retrieved after delete")
    (is (empty? items) "The reconciliation is not associated with any items after delete")))

(deftest a-reconciliation-that-is-not-the-most-recent-cannot-be-deleted
  (let [context (serialization/realize storage-spec working-reconciliation-context)
        reconciliation (-> context :reconciliations first)]
    (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
             (reconciliations/delete storage-spec (:id reconciliation)))
        "an exception should be thrown")
    (is (reconciliations/find-by-id storage-spec (:id reconciliation)) "The reconciliation can still be retrieved")
    (is (not (empty? (transactions/select-items-by-reconciliation storage-spec reconciliation)))
        "The transaction items are still associated with the reconciliation")))

(deftest check-if-a-transaction-can-be-deleted
  (let [context (serialization/realize storage-spec existing-reconciliation-context)
        [t1 t2] (->> context
                     :transactions
                     (take 2)
                     (map #(transactions/reload storage-spec %)))]
    (is (not (transactions/can-delete? t1))
        "A transaction with a reconciled item cannot be deleted")
    (is (transactions/can-delete? t2)
        "A transaction with no reconciled items can be deleted")))
