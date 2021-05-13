(ns clj-money.models.reconciliations-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clj-time.core :as t]
            [dgknght.app-lib.test]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [realize
                                            basic-context
                                            find-recon
                                            find-account
                                            find-transaction-item]]
            [clj-money.models.reconciliations :as reconciliations]
            [clj-money.models.transactions :as transactions]))

(use-fixtures :each reset-db)

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
                   :debit-account-id "Checking"
                   :credit-account-id "Salary"
                   :quantity 1000M}
                  {:transaction-date (t/local-date 2017 1 2)
                   :description "Landlord"
                   :debit-account-id "Rent"
                   :credit-account-id "Checking"
                   :quantity 500M}
                  {:transaction-date (t/local-date 2017 1 3)
                   :description "Kroger"
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"
                   :quantity 45M}
                  {:transaction-date (t/local-date 2017 1 10)
                   :description "Safeway"
                   :debit-account-id "Groceries"
                   :credit-account-id "Checking"
                   :quantity 53M}]})

(def ^:private existing-reconciliation-context
  (assoc reconciliation-context
         :reconciliations
         [{:account-id "Checking"
           :end-of-period (t/local-date 2017 1 1)
           :balance 1000M
           :status :completed
           :item-refs [{:transaction-date (t/local-date 2017 1 1)
                        :quantity 1000M}]}]))

(def ^:private working-reconciliation-context
  (update-in existing-reconciliation-context
             [:reconciliations]
             #(conj % {:account-id "Checking"
                       :end-of-period (t/local-date 2017 1 3)
                       :balance 455M
                       :status :new
                       :item-refs [{:transaction-date (t/local-date 2017 1 2)
                                    :quantity 500M}]})))

(deftest create-a-reconciliation
  (let [context (realize reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period (t/local-date 2017 1 31)}
        result (reconciliations/create reconciliation)
        retrieved (reconciliations/find-by {:account-id (:id checking)})]
    (is (valid? result))
    (is retrieved "The reconciliation can be retrieved")
    (testing "transaction items are not marked as reconciled"
      (is (= #{false}
             (->> (:transactions context)
                  (map transactions/reload)
                  (mapcat :items)
                  (filter #(= (:id checking) (:account-id %)))
                  (map :reconciled?)
                  set))
          "None of the transaction items should be marked as reconcilied"))))

(deftest create-a-completed-reconciliation
  (let [context (realize reconciliation-context)
        checking (-> context :accounts first)
        [paycheck
         landlord
         _
         safeway] (->> context
                       :transactions
                       (mapcat :items)
                       (filter #(= (:id checking) (:account-id %))))
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period (t/local-date 2017 1 31)
                        :item-refs (map (juxt :id :transaction-date)
                                        [paycheck landlord safeway])
                        :status :completed}
        result (reconciliations/create reconciliation)
        retrieved (reconciliations/find-by {:account-id (:id checking)})]
    (is (valid? result))
    (is retrieved "The reconciliation can be retrieved")
    (testing "transaction items are marked as reconciled"
      (is (= [true true false true]
             (->> (:transactions context)
                  (map transactions/reload)
                  (mapcat :items)
                  (filter #(= (:id checking) (:account-id %)))
                  (map :reconciled?)))
          "Each transaction item included in the reconciliation should be marked as reconciled"))))

(deftest a-new-reconciliation-cannot-be-created-if-one-already-exists
  (let [context (realize working-reconciliation-context)
        checking (-> context :accounts first)
        result (reconciliations/create {:account-id (:id checking)
                                        :balance 1M
                                        :status :new
                                        :end-of-period (t/local-date 2017 2 28)})]
    (is (invalid? result [:account-id] "Account already has a reconciliation in progress"))))

(deftest account-id-is-required
  (let [context (realize reconciliation-context)
        checking (-> context :accounts first)
        [paycheck
         landlord
         _
         safeway] (->> context
                       :transactions
                       (mapcat :items)
                       (filter #(= (:id checking) (:account-id %))))
        result (reconciliations/create {:balance 447M
                                        :item-refs (map (juxt :id :transaction-date)
                                                        [paycheck landlord safeway])})]
    (is (invalid? result [:account-id] "Account is required"))))

(deftest end-of-period-is-required
  (let [context (realize reconciliation-context)
        checking (-> context :accounts first)
        [paycheck
         landlord
         _
         safeway] (->> context
                       :transactions
                       (mapcat :items)
                       (filter #(= (:id checking) (:account-id %))))
        result (reconciliations/create {:balance 447M
                                        :item-refs (map (juxt :id :transaction-date)
                                                        [paycheck landlord safeway])})]
    (is (invalid? result [:end-of-period] "End of period is required"))))

(deftest end-of-period-must-come-after-the-previous-end-of-period
  (let [context (realize existing-reconciliation-context)
        checking (-> context :accounts first)
        reconciliation {:account-id (:id checking)
                        :balance 447M
                        :end-of-period (t/local-date 2016 12 31)}
        result (reconciliations/create reconciliation)]
    (is (invalid? result [:end-of-period] "End of period must be after that latest reconciliation"))))

(deftest status-must-be-new-or-completed
  (let [context (realize reconciliation-context)
        checking (-> context :accounts first)
        [paycheck
         landlord
         _
         safeway] (->> context
                       :transactions
                       (mapcat :items)
                       (filter #(= (:id checking) (:account-id %))))
        result (reconciliations/create {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :status :bouncy
                                        :balance 447M
                                        :item-refs (map (juxt :id :transaction-date)
                                                        [paycheck landlord safeway])})]
    (is (invalid? result [:status] "Status must be new or completed"))))

(deftest item-refs-cannot-reference-items-that-belong-to-the-account-being-reconciled
  (let [context (realize reconciliation-context)
        [checking
         salary] (:accounts context)
        paycheck (->> context
                      :transactions
                      (mapcat :items)
                      (filter #(= (:id salary) (:account-id %)))
                      first)
        result (reconciliations/create {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :balance 10M
                                        :item-refs [((juxt :id :transaction-date) paycheck)]})]
    (is (invalid? result [:item-refs] "All items must belong to the account being reconciled"))))

(def ^:private parent-account-context
  (-> basic-context
      (update-in [:accounts] concat [{:name "Savings"
                                      :type :asset
                                      :entity-id "Personal"}
                                     {:name "Car"
                                      :type :asset
                                      :entity-id "Personal"
                                      :parent-id "Savings"}
                                     {:name "Reserve"
                                      :type :asset
                                      :entity-id "Personal"
                                      :parent-id "Savings"}])
      (assoc :transactions [{:transaction-date (t/local-date 2015 1 1)
                             :description "Paycheck"
                             :items [{:action :credit
                                      :account-id "Salary"
                                      :quantity 1000M}
                                     {:action :debit
                                      :account-id "Car"
                                      :quantity 100M}
                                     {:action :debit
                                      :account-id "Reserve"
                                      :quantity 200M}
                                     {:action :debit
                                      :account-id "Checking"
                                      :quantity 700M}]}])))

(deftest item-refs-can-reference-items-that-belong-to-children-of-the-account-being-reconciled
  (let [ctx (realize parent-account-context)
        account (find-account ctx "Savings")
        items (transactions/search-items {:account-id (:id account)
                                          :transaction-date [:between
                                                             (t/local-date 2015 1 1)
                                                             (t/local-date 2015 1 31)]}
                                         {:include-children? true})
        result (reconciliations/create {:account-id (:id account)
                                        :end-of-period (t/local-date 2015 1 31)
                                        :status :completed
                                        :balance 300M
                                        :item-refs (map (juxt :id :transaction-date) items)})]
    (is (valid? result))))

(def ^:private working-rec-context
  (assoc reconciliation-context
         :reconciliations
         [{:account-id "Checking"
           :end-of-period (t/local-date 2017 1 1)
           :balance 447M
           :status :new
           :item-refs [{:transaction-date (t/local-date 2017 1 1)
                        :account-id "Salary"
                        :quantity 1000M}
                       {:transaction-date (t/local-date 2017 1 2)
                        :account-id "Rent"
                        :quantity 500M}
                       {:transaction-date (t/local-date 2017 1 10)
                        :account-id "Groceries"
                        :quantity 53M}]}]))

(deftest find-the-working-reconciliation
  (let [context (realize working-rec-context)
        checking (-> context :accounts first)
        reconciliation (reconciliations/find-working (:id checking))]
    (is reconciliation "A reconciliation is returned")))

(deftest transaction-item-can-only-belong-to-one-reconciliation
  (let [context (realize existing-reconciliation-context)
        checking (find-account context "Checking")
        item (->> (:transactions context)
                  (mapcat :items)
                  (filter #(= (:id checking) (:account-id %)))
                  first)
        result (reconciliations/create {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :balance 1500M
                                        :item-refs [((juxt :id :transaction-date) item)]})]
    (is (invalid? result [:item-refs] "No item can belong to another reconciliation"))))

(deftest a-working-reconciliation-can-be-updated
  (let [context (realize working-reconciliation-context)
        recon (-> context :reconciliations last)
        result (reconciliations/update
                (assoc recon :balance 1499M))
        retrieved (reconciliations/find recon)]
    (is (valid? result))
    (is (= 1499M (:balance retrieved))
        "The retrieved value has the correct balance after update")))

(deftest a-working-reconciliation-can-be-completed
  (let [context (realize working-reconciliation-context)
        previous-rec (find-recon context "Checking" (t/local-date 2017 1 1))
        reconciliation (find-recon context "Checking" (t/local-date 2017 1 3))
        checking (find-account context "Checking")
        item (find-transaction-item context
                                    (t/local-date 2017 1 3)
                                    45M
                                    (:id checking))
        updated (-> reconciliation
                    (assoc :status :completed)
                    (update-in [:item-refs]
                               conj
                               ((juxt :id :transaction-date) item)))
        result (reconciliations/update updated)
        retrieved (reconciliations/reload updated)
        checking-items (transactions/search-items {:account-id (:id checking)
                                                   :transaction-date "2017"}
                                                  {:sort [:transaction-date]})]
    (is (valid? result))
    (is (= (:status retrieved) :completed) "The retrieved value has the correct satus")
    (testing "reconciled transaction items"
      (let [expected [{:transaction-date (t/local-date 2017 1 1)
                       :quantity 1000M
                       :reconciliation-id (:id previous-rec)}
                      {:transaction-date (t/local-date 2017 1 2)
                       :quantity 500M
                       :reconciliation-id (:id retrieved)}
                      {:transaction-date (t/local-date 2017 1 3)
                       :quantity 45M
                       :reconciliation-id (:id retrieved)}
                      {:transaction-date (t/local-date 2017 1 10)
                       :quantity 53M
                       :reconciliation-id nil}]
            actual (map #(select-keys % [:transaction-date :quantity :reconciliation-id])
                        checking-items)]
        (is (= expected actual)
            "The correct transaction items are associated with the reconciliation")))))

(deftest cannot-create-a-completed-out-of-balance-reconciliation
  (let [context (realize reconciliation-context)
        checking (find-account context "Checking")
        result (reconciliations/create {:account-id (:id checking)
                                        :end-of-period (t/local-date 2017 1 31)
                                        :balance 101M
                                        :status :completed})]
    (is (invalid? result [:balance] "Balance must match the calculated balance"))))

(deftest an-out-of-balance-reconciliation-cannot-be-updated-to-completed
  (let [context (realize working-reconciliation-context)
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
        result (reconciliations/update updated)]
    (is (invalid? result [:balance] "Balance must match the calculated balance"))))

(deftest a-completed-reconciliation-cannot-be-updated
  (let [context (realize existing-reconciliation-context)
        reconciliation (-> context :reconciliations first)
        result (reconciliations/update (assoc reconciliation :end-of-period (t/local-date 2017 1 2)))
        retrieved (reconciliations/reload reconciliation)]
    (is (invalid? result [:status] "A completed reconciliation cannot be updated"))
    (is (= 1000M (:balance retrieved)) "The new value is not saved")))

(deftest the-most-recent-completed-reconciliation-can-be-deleted
  (let [context (realize existing-reconciliation-context)
        reconciliation (-> context :reconciliations first)
        _ (reconciliations/delete reconciliation)
        retrieved (reconciliations/find reconciliation)
        items (transactions/select-items-by-reconciliation reconciliation)]
    (is (nil? retrieved) "The reconciliation cannot be retrieved after delete")
    (is (empty? items) "The reconciliation is not associated with any items after delete")))

(deftest a-working-reconciliation-can-be-deleted
  (let [context (realize working-reconciliation-context)
        reconciliation (-> context :reconciliations second)
        _ (reconciliations/delete reconciliation)
        retrieved (reconciliations/find reconciliation)
        items (transactions/select-items-by-reconciliation reconciliation)]
    (is (nil? retrieved) "The reconciliation cannot be retrieved after delete")
    (is (empty? items) "The reconciliation is not associated with any items after delete")))

(deftest a-reconciliation-that-is-not-the-most-recent-cannot-be-deleted
  (let [context (realize working-reconciliation-context)
        reconciliation (-> context :reconciliations first)]
    (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
                          (reconciliations/delete reconciliation))
        "an exception should be thrown")
    (is (reconciliations/find reconciliation) "The reconciliation can still be retrieved")
    (is (seq (transactions/select-items-by-reconciliation reconciliation))
        "The transaction items are still associated with the reconciliation")))

(deftest check-if-a-transaction-can-be-deleted
  (let [context (realize existing-reconciliation-context)
        [t1 t2] (->> context
                     :transactions
                     (take 2)
                     (map transactions/reload))]
    (is (not (transactions/can-delete? t1))
        "A transaction with a reconciled item cannot be deleted")
    (is (transactions/can-delete? t2)
        "A transaction with no reconciled items can be deleted")))
