(ns clj-money.models.reconciliations-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.util :as util :refer [model=
                                             ->model-ref]]
            [clj-money.json]
            [clj-money.db :as db]
            [clj-money.db.sql.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.accounts :as acts]
            [clj-money.model-helpers
             :refer [assert-invalid
                     assert-deleted]
             :as helpers]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.test-context :refer [with-context
                                            *context*
                                            basic-context
                                            find-entity
                                            find-account
                                            find-transaction-item
                                            find-reconciliation]]
            [clj-money.models.reconciliations :as recons]))

(use-fixtures :each reset-db)

(def ^:private ->item-ref
  (juxt :id :transaction-item/transaction-date))

(def ^:private reconciliation-context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2017 1 2)
                      :entity "Personal"
                      :description "Landlord"
                      :debit-account "Rent"
                      :credit-account "Checking"
                      :quantity 500M}
        #:transaction{:transaction-date (t/local-date 2017 1 3)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 45M}
        #:transaction{:transaction-date (t/local-date 2017 1 10)
                      :entity "Personal"
                      :description "Safeway"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 53M}))

(def ^:private existing-reconciliation-context
  (conj reconciliation-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 1)
                         :balance 1000M
                         :status :completed
                         :item-refs [[(t/local-date 2017 1 1)
                                      1000M]]}))

(def ^:private working-reconciliation-context
  (conj existing-reconciliation-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 3)
                         :balance 455M
                         :status :new
                         :item-refs [[(t/local-date 2017 1 2)
                                      500M]]}))

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:reconciliation/account]
                          :compare-result? false))

(defn- attributes []
  #:reconciliation{:account (find-account "Checking")
                   :balance 447M
                   :end-of-period (t/local-date 2017 1 31)})

(deftest create-a-reconciliation
  (with-context reconciliation-context
    (assert-created (attributes))
    (testing "transaction items are not marked as reconciled"
      (is (->> (models/select {:transaction/entity (find-entity "Personal")})
                  (mapcat :transaction/items)
                  (not-any? :transaction/recondiliation))
          "None of the transaction items should be marked as reconcilied"))))

(deftest create-a-completed-reconciliation
  (with-context reconciliation-context
    (let [checking (find-account "Checking")
          checking-items (models/select {:transaction-item/account checking
                                         :transaction-item/quantity [:!= 45]})]
      (assert-created (assoc (attributes)
                             :reconciliation/item-refs (map ->item-ref checking-items)
                             :reconciliation/status :completed))
      (is (->> checking-items
               (mapcat :transaction/items)
               (every? :transaction/recondiliation))
          "specified transaction items are marked as reconciled")
      (is (not-any? :transaction/reconciliation
                    (remove #(util/model= checking (:transaction-item/account %))
                            (models/select
                              (db/model-type
                                {:transaction/entity (find-entity "Personal")}
                                :transaction-item))))
          "All other transaction items are not marked as reconcilied"))))

(deftest a-new-reconciliation-cannot-be-created-if-one-already-exists
  (with-context working-reconciliation-context
    (assert-invalid
      (assoc (attributes) :reconciliation/status :new)
      {:reconciliation/account ["Account already has a reconciliation in progress"]})))

(deftest account-is-required
  (with-context reconciliation-context
    (assert-invalid (dissoc (attributes) :reconciliation/account)
                    {:reconciliation/account ["Account is required"]})))

(deftest end-of-period-is-required
  (with-context reconciliation-context
    (assert-invalid
      (dissoc (attributes) :reconciliation/end-of-period)
      {:reconciliation/end-of-period ["End of period is required"]})))

(deftest end-of-period-must-come-after-the-previous-end-of-period
  (with-context existing-reconciliation-context
    (assert-invalid
      (assoc (attributes)
             :reconciliation/end-of-period (t/local-date 2016 12 31))
      {:reconciliation/end-of-period ["End of period must be after that latest reconciliation"]})))

(deftest status-must-be-new-or-completed
  (with-context existing-reconciliation-context
    (assert-invalid
      (assoc (attributes)
             :reconciliation/status :bouncy)
      {:reconciliation/status ["Status must be new or completed"]})))

(deftest item-refs-cannot-reference-items-that-belong-to-the-account-being-reconciled
  (with-context reconciliation-context
    (assert-invalid #:reconciliation{:account (find-account "Groceries")
                                     :end-of-period (t/local-date 2017 1 31)
                                     :balance 500M
                                     :item-refs [(->item-ref
                                                  (find-transaction-item
                                                    [(t/local-date 2017 1 2)
                                                     500M
                                                     (find-account "Rent")]))]}
                    {:reconciliation/item-refs ["All items must belong to the account being reconciled"]})))

(def ^:private parent-account-context
  (conj basic-context
        #:account{:name "Savings"
                  :type :asset
                  :entity "Personal"}
        #:account{:name "Car"
                  :type :asset
                  :entity "Personal"
                  :parent "Savings"}
        #:account{:name "Reserve"
                  :type :asset
                  :entity "Personal"
                  :parent "Savings"}
        #:transaction{:transaction-date (t/local-date 2015 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1000M}
                              #:transaction-item{:action :debit
                                                 :account "Car"
                                                 :quantity 100M}
                              #:transaction-item{:action :debit
                                                 :account "Reserve"
                                                 :quantity 200M}
                              #:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 700M}]}))

(deftest item-refs-can-reference-items-that-belong-to-children-of-the-account-being-reconciled
  (with-context parent-account-context
    (let [savings (find-account "Savings")
          car (find-account "Car")
          reserve (find-account "Reserve")]
      (assert-created
        #:reconciliation{:account savings
                         :end-of-period (t/local-date 2015 1 31)
                         :status :completed
                         :balance 300M
                         :item-refs (->> *context*
                                         (filter (db/model-type? :transaction))
                                         (mapcat :transaction/items)
                                         (filter #(or (model= reserve
                                                              (:transaction-item/account %))
                                                      (model= car
                                                              (:transaction-item/account %))))
                                         (mapv ->item-ref))}))))

(def ^:private working-rec-context
  (conj reconciliation-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 1)
                         :balance 447M
                         :status :new
                         :item-refs [[(t/local-date 2017 1 1)
                                      1000M]
                                     [(t/local-date 2017 1 2)
                                      500M]
                                     [(t/local-date 2017 1 10)
                                      53M]]}))

(deftest find-the-working-reconciliation
  (with-context working-rec-context
    (is (comparable? #:reconciliation{:balance 447M}
                     (recons/find-working (find-account "Checking"))))))

(deftest transaction-item-can-only-belong-to-one-reconciliation
  (with-context existing-reconciliation-context
    (assert-invalid
      #:reconciliation{:account (find-account "Checking")
                       :end-of-period (t/local-date 2017 1 31)
                       :balance 1500M
                       :item-refs [(->item-ref
                                    (find-transaction-item
                                      [(t/local-date 2017 1 1)
                                       1000M
                                       "Checking"]))]}
      {:reconciliation/item-refs ["No item can belong to another reconciliation"]})))

(deftest a-working-reconciliation-can-be-updated
  (with-context working-reconciliation-context
    (let [result (-> (find-reconciliation ["Checking"
                                           (t/local-date 2017 1 3)])
                     (assoc :reconciliation/balance 1499M)
                     models/put)]
      (is (comparable? {:reconciliation/balance 1499M}
                       result)
          "The result has the correct balance after update")
      (is (comparable? {:reconciliation/balance 1499M}
                       (models/find result))
          "The retrieved value has the correct balance after update"))))

(deftest a-working-reconciliation-can-be-completed
  (with-context working-reconciliation-context
    (let [checking (find-account "Checking")
          previous-rec (find-reconciliation [checking (t/local-date 2017 1 1)])
          item-ref (->item-ref
                    (find-transaction-item [(t/local-date 2017 1 3)
                                            45M
                                            checking]))
          result (-> (find-reconciliation [checking (t/local-date 2017 1 3)])
                     (assoc :reconciliation/status :completed)
                     (update-in [:reconciliation/item-refs] conj item-ref)
                     models/put)]
      (is (comparable? #:reconciliation {:status :completed}
                       result)
          "The result reflects the updated attributes")
      (is (comparable? #:reconciliation{:status :completed}
                       (models/find result :reconciliation))
          "The retrieved record reflects the updated attributes")
      (is (seq-of-maps-like? [#:transaction-item{:transaction-date (t/local-date 2017 1 1)
                                                 :quantity 1000M
                                                 :reconciliation (->model-ref previous-rec)}
                              #:transaction-item{:transaction-date (t/local-date 2017 1 2)
                                                 :quantity 500M
                                                 :reconciliation (->model-ref result)}
                              #:transaction-item{:transaction-date (t/local-date 2017 1 3)
                                                 :quantity 45M
                                                 :reconciliation (->model-ref result)}
                              #:transaction-item{:transaction-date (t/local-date 2017 1 10)
                                                 :quantity 53M
                                                 :reconciliation nil}]
                             (models/select (-> checking models/find acts/->criteria)))
          "The retrieved transaction items have the new reconciliation reference"))))

(deftest cannot-create-a-completed-out-of-balance-reconciliation
  (with-context reconciliation-context
    (assert-invalid #:reconciliation{:account (find-account "Checking")
                                     :end-of-period (t/local-date 2017 1 31)
                                     :balance 101M
                                     :status :completed}
                    {:reconciliation/balance ["Balance must match the calculated balance"]})))

(deftest an-out-of-balance-reconciliation-cannot-be-updated-to-completed
  (with-context working-reconciliation-context
    (let [item-ref (->item-ref (find-transaction-item [(t/local-date 2017 1 10)
                                                       53M
                                                       "Checking"]))]
      (-> (find-reconciliation ["Checking" (t/local-date 2017 1 3)])
          (assoc :reconciliation/status :completed)
          (update-in [:reconciliation/item-refs]
                     conj
                     item-ref)
          (assert-invalid {:reconciliation/balance ["Balance must match the calculated balance"]})))))

(deftest a-completed-reconciliation-cannot-be-updated
  (with-context existing-reconciliation-context
    (assert-invalid (assoc (find-reconciliation ["Checking" (t/local-date 2017 1 1)])
                           :reconciliation/end-of-period (t/local-date 2017 1 31))
                    {:reconciliation/status ["A completed reconciliation cannot be updated"]})))

(deftest the-most-recent-completed-reconciliation-can-be-deleted
  (with-context existing-reconciliation-context
    (let [reconciliation (find-reconciliation ["Checking" (t/local-date 2017 1 1)])]
      (assert-deleted reconciliation)
      (is (empty? (models/select
                    (db/model-type
                      {:transaction-item/reconciliation (->model-ref reconciliation)
                       :transaction/transaction-date [:between (t/local-date 2016 1 1) (t/local-date 2017 1 31)]}
                      :transaction-item)))
          "The reconciliation is not associated with any items after delete"))))

(deftest a-working-reconciliation-can-be-deleted
  (with-context working-reconciliation-context
    (let [reconciliation (find-reconciliation ["Checking" (t/local-date 2017 1 3)])]
      (assert-deleted reconciliation)
      (is (empty? (models/select
                    (db/model-type
                      {:transaction-item/reconciliation (->model-ref reconciliation)
                       :transaction/transaction-date [:between (t/local-date 2016 1 1) (t/local-date 2017 1 31)]}
                      :transaction-item)))
          "The reconciliation is not associated with any items after delete"))))

(deftest a-reconciliation-that-is-not-the-most-recent-cannot-be-deleted
  (with-context working-reconciliation-context
    (let [reconciliation (find-reconciliation ["Checking" (t/local-date 2017 1 1)])]
      (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
                            (models/delete reconciliation))
          "an exception is thrown")
      (is (models/find reconciliation) "The reconciliation can still be retrieved")
      (is (seq (models/select
                 (db/model-type
                   {:transaction-item/reconciliation (->model-ref reconciliation)
                    :transaction/transaction-date [:between (t/local-date 2016 1 1) (t/local-date 2017 1 31)]}
                   :transaction-item)))
          "The transaction items are still associated with the reconciliation"))))
