(ns clj-money.entities.reconciliations-test
  (:require [clojure.test :refer [is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.util :as util :refer [entity=
                                             ->entity-ref]]
            [clj-money.json]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [dbtest]]
            [clj-money.accounts :as acts]
            [clj-money.entity-helpers
             :refer [assert-invalid
                     assert-deleted]
             :as helpers]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.entities.ref]
            [clj-money.test-context :refer [with-context
                                            *context*
                                            basic-context
                                            find-entity
                                            find-account
                                            find-account-item
                                            find-reconciliation]]
            [clj-money.entities.reconciliations :as recons]))

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
                         :items [[(t/local-date 2017 1 1)
                                  1000M]]}))

(def ^:private working-reconciliation-context
  (conj existing-reconciliation-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 3)
                         :balance 455M
                         :status :new
                         :items [[(t/local-date 2017 1 2)
                                  500M]]}))

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:reconciliation/account]
                          :ignore-attributes [:reconciliation/items]
                          :compare-result? false))

(defn- attributes []
  #:reconciliation{:account (find-account "Checking")
                   :balance 447M
                   :end-of-period (t/local-date 2017 1 31)})

(dbtest create-a-reconciliation
  (with-context reconciliation-context
    (assert-created (attributes))
    (testing "transaction items are not marked as reconciled"
      (is (->> (entities/select {:transaction/entity (find-entity "Personal")})
                  (mapcat :transaction/items)
                  (not-any? :transaction/recondiliation))
          "None of the transaction items should be marked as reconcilied"))))

(dbtest create-a-completed-reconciliation
  (with-context reconciliation-context
    (let [checking (find-account "Checking")
          checking-items (entities/select
                           {:account-item/account checking
                            :account-item/quantity [:!= -45M]}
                           {:select-also [:transaction/transaction-date]})]

      (assert-created (assoc (attributes)
                             :reconciliation/items checking-items
                             :reconciliation/status :completed))

      (is (every? :account-item/reconciliation
                  (entities/select {:account-item/account checking
                                    :account-item/quantity [:!= -45M]}))
          "specified items are marked as reconciled")
      (is (not-any? :account-item/reconciliation
                    (entities/select {:account-item/account checking
                                      :account-item/quantity -45M}))
          "Non-specified items in the same account are not marked")
      (is (not-any? :account-item/reconciliation
                    (entities/select {:account-item/account [:!= checking]}))
          "Items in other accounts are not marked"))))

(dbtest a-new-reconciliation-cannot-be-created-if-one-already-exists
  (with-context working-reconciliation-context
    (assert-invalid
      (assoc (attributes) :reconciliation/status :new)
      {:reconciliation/account ["Account already has a reconciliation in progress"]})))

(dbtest account-is-required
  (with-context reconciliation-context
    (assert-invalid (dissoc (attributes) :reconciliation/account)
                    {:reconciliation/account ["Account is required"]})))

(dbtest end-of-period-is-required
  (with-context reconciliation-context
    (assert-invalid
      (dissoc (attributes) :reconciliation/end-of-period)
      {:reconciliation/end-of-period ["End of period is required"]})))

(dbtest end-of-period-must-come-after-the-previous-end-of-period
  (with-context existing-reconciliation-context
    (assert-invalid
      (assoc (attributes)
             :reconciliation/end-of-period (t/local-date 2016 12 31))
      {:reconciliation/end-of-period ["End of period must be after that latest reconciliation"]})))

(dbtest status-must-be-new-or-completed
  (with-context existing-reconciliation-context
    (assert-invalid
      (assoc (attributes)
             :reconciliation/status :bouncy)
      {:reconciliation/status ["Status must be new or completed"]})))

(dbtest items-must-belong-to-the-account-being-reconciled
  (with-context reconciliation-context
    (assert-invalid #:reconciliation{:account (find-account "Groceries")
                                     :end-of-period (t/local-date 2017 1 31)
                                     :balance 500M
                                     :items [(find-account-item
                                               [(t/local-date 2017 1 2)
                                                500M
                                                (find-account "Rent")])]}
                    {:reconciliation/items ["All items must belong to the account being reconciled"]})))

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

(dbtest items-can-belong-to-children-of-the-account-being-reconciled
  (with-context parent-account-context
    (let [savings (find-account "Savings")
          car (find-account "Car")
          reserve (find-account "Reserve")
          items (->> *context*
                     (filter (util/entity-type? :transaction))
                     (mapcat :transaction/items)
                     (filter #(or (entity= reserve
                                          (:transaction-item/account %))
                                  (entity= car
                                          (:transaction-item/account %)))))
          _ (assert (= 2 (count items)) "Expected 2 items for the test")
          created (assert-created
                    #:reconciliation{:account savings
                                     :end-of-period (t/local-date 2015 1 31)
                                     :status :completed
                                     :balance 300M
                                     :items items})
          simplify #(select-keys % [:transaction-item/action
                                    :transaction-item/account
                                    :transaction-item/quantity])
          retrieved (entities/select {:transaction-item/reconciliation created})]
      (is (= (->> items
                  (map simplify)
                  set)
             (->> retrieved
                  (map simplify)
                  set))
          "The items are updated with a reference to the reconciliation"))))

(def ^:private working-rec-context
  (conj reconciliation-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 1)
                         :balance 447M
                         :status :new
                         :items [[(t/local-date 2017 1 1)
                                      1000M]
                                     [(t/local-date 2017 1 2)
                                      500M]
                                     [(t/local-date 2017 1 10)
                                      53M]]}))

(dbtest find-the-working-reconciliation
  (with-context working-rec-context
    (is (comparable? #:reconciliation{:balance 447M}
                     (recons/find-working (find-account "Checking"))))))

(dbtest account-item-can-only-belong-to-one-reconciliation
  (with-context existing-reconciliation-context
    (assert-invalid
      #:reconciliation{:account (find-account "Checking")
                       :end-of-period (t/local-date 2017 1 31)
                       :balance 1500M
                       :items (entities/select
                                (util/entity-type
                                  {:transaction/transaction-date (t/local-date 2017 1 1)
                                   :transaction-item/quantity 1000M
                                   :account/name "Checking"}
                                  :transaction-item)) }
      {:reconciliation/items ["No item can belong to another reconciliation"]})))

(dbtest a-working-reconciliation-can-be-updated
  (with-context working-reconciliation-context
    (let [result (-> (find-reconciliation ["Checking"
                                           (t/local-date 2017 1 3)])
                     (assoc :reconciliation/balance 1499M)
                     entities/put)]
      (is (comparable? {:reconciliation/balance 1499M}
                       result)
          "The result has the correct balance after update")
      (is (comparable? {:reconciliation/balance 1499M}
                       (entities/find result))
          "The retrieved value has the correct balance after update"))))

(dbtest a-working-reconciliation-can-be-completed
  (with-context working-reconciliation-context
    (let [checking (find-account "Checking")
          previous-rec (find-reconciliation [checking (t/local-date 2017 1 1)])
          item (find-account-item [(t/local-date 2017 1 3)
                                   45M
                                   checking])
          result (-> (find-reconciliation [checking (t/local-date 2017 1 3)])
                     (assoc :reconciliation/status :completed)
                     (update-in [:reconciliation/items] conj item)
                     entities/put)]
      (is (comparable? #:reconciliation {:status :completed}
                       result)
          "The result reflects the updated attributes")
      (is (comparable? #:reconciliation{:status :completed}
                       (entities/find result))
          "The retrieved record reflects the updated attributes")
      (is (seq-of-maps-like?
            [{:transaction/transaction-date (t/local-date 2017 1 1)
              :account-item/quantity 1000M
              :account-item/reconciliation (->entity-ref previous-rec)}
             {:transaction/transaction-date (t/local-date 2017 1 2)
              :account-item/quantity -500M
              :account-item/reconciliation (->entity-ref result)}
             {:transaction/transaction-date (t/local-date 2017 1 3)
              :account-item/quantity -45M
              :account-item/reconciliation (->entity-ref result)}
             {:transaction/transaction-date (t/local-date 2017 1 10)
              :account-item/quantity -53M
              :account-item/reconciliation nil}]
            (map #(update-in %
                             [:account-item/reconciliation]
                             identity)
                 (entities/select
                   (-> checking entities/find acts/->criteria)
                   {:sort [:transaction/transaction-date]
                    :select-also [:transaction/transaction-date]})))
          "The retrieved transaction items have the new reconciliation reference"))))

(dbtest cannot-create-a-completed-out-of-balance-reconciliation
  (with-context reconciliation-context
    (assert-invalid #:reconciliation{:account (find-account "Checking")
                                     :end-of-period (t/local-date 2017 1 31)
                                     :balance 101M
                                     :status :completed}
                    {:reconciliation/balance ["Balance must match the calculated balance"]})))

(dbtest an-out-of-balance-reconciliation-cannot-be-updated-to-completed
  (with-context working-reconciliation-context
    (let [item (find-account-item [(t/local-date 2017 1 10)
                                   53M
                                   "Checking"])]
      (-> (find-reconciliation ["Checking" (t/local-date 2017 1 3)])
          (assoc :reconciliation/status :completed)
          (update-in [:reconciliation/items]
                     conj
                     item)
          (assert-invalid {:reconciliation/balance ["Balance must match the calculated balance"]})))))

(dbtest a-completed-reconciliation-cannot-be-updated
  (with-context existing-reconciliation-context
    (assert-invalid (assoc (find-reconciliation ["Checking" (t/local-date 2017 1 1)])
                           :reconciliation/end-of-period (t/local-date 2017 1 31))
                    {:reconciliation/status ["A completed reconciliation cannot be updated"]})))

(dbtest the-most-recent-completed-reconciliation-can-be-deleted
  (with-context existing-reconciliation-context
    (assert-deleted (find-reconciliation ["Checking" (t/local-date 2017 1 1)]))))

(dbtest a-working-reconciliation-can-be-deleted
  (with-context working-reconciliation-context
    (assert-deleted (find-reconciliation ["Checking" (t/local-date 2017 1 3)]))))

(dbtest ^:multi-threaded propagate-reconciliation-deletion
  (with-context working-reconciliation-context
    (let [reconciliation (find-reconciliation ["Checking" (t/local-date 2017 1 3)])]
      (prop/delete-and-propagate reconciliation)
      (is (empty? (entities/select
                    (util/entity-type
                      {:transaction-item/reconciliation (->entity-ref reconciliation)
                       :transaction/transaction-date [:between (t/local-date 2016 1 1) (t/local-date 2017 1 31)]}
                      :transaction-item)))
          "The reconciliation is not associated with any items after delete"))))

(dbtest a-reconciliation-that-is-not-the-most-recent-cannot-be-deleted
  (with-context working-reconciliation-context
    (let [reconciliation (find-reconciliation ["Checking" (t/local-date 2017 1 1)])]
      (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
                            (entities/delete reconciliation))
          "an exception is thrown")
      (is (entities/find reconciliation)
          "The reconciliation can still be retrieved"))))

(dbtest ^:multi-threaded a-failed-attempt-to-delete-does-not-propagate
  (with-context working-reconciliation-context
    (let [reconciliation (find-reconciliation ["Checking" (t/local-date 2017 1 1)])]
      (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
                            (prop/delete-and-propagate reconciliation))
          "an exception is thrown during propagation")
      (is (seq (entities/select
                 (util/entity-type
                   {:transaction-item/reconciliation (->entity-ref reconciliation)
                    :transaction/transaction-date [:between (t/local-date 2016 1 1) (t/local-date 2017 1 31)]}
                   :transaction-item)))
          "The transaction items are still associated with the reconciliation"))))
