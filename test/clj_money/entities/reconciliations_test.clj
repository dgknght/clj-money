(ns clj-money.entities.reconciliations-test
  (:require [clojure.test :refer [is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.util :as util :refer [id=
                                             ->entity-ref]]
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
                                            find-accounts
                                            find-transaction-item
                                            find-reconciliation]]
            [clj-money.entities.reconciliations :as recs]))

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

(def ^:private working-recon-context
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
                           {:transaction-item/account checking
                            :transaction-item/quantity [:!= 45M]}
                           {:select-also [:transaction/transaction-date]})]

      (assert-created (assoc (attributes)
                             :reconciliation/items checking-items
                             :reconciliation/status :completed))

      (is (every? :transaction-item/reconciliation
                  (entities/select {:transaction-item/account checking
                                    :transaction-item/quantity [:!= 45M]}))
          "specified items are marked as reconciled")
      (is (not-any? :transaction-item/reconciliation
                    (entities/select {:transaction-item/account checking
                                      :transaction-item/quantity 45M}))
          "Non-specified items in the same account are not marked")
      (is (not-any? :transaction-item/reconciliation
                    (entities/select {:transaction-item/account [:!= checking]}))
          "Items in other accounts are not marked"))))

(dbtest a-new-reconciliation-cannot-be-created-if-one-already-exists
  (with-context working-recon-context
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
                                     :items [(find-transaction-item
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
                      :items [#:transaction-item{:quantity 1000M
                                                 :action :credit
                                                 :account "Salary"}
                              #:transaction-item{:quantity 100M
                                                 :action :debit
                                                 :account "Car"}
                              #:transaction-item{:quantity 200M
                                                 :action :debit
                                                 :account "Reserve"}
                              #:transaction-item{:quantity 700M
                                                 :action :debit
                                                 :account "Checking"}]}))

(dbtest items-can-belong-to-children-of-the-account-being-reconciled
  (with-context parent-account-context
    (let [[savings car reserve] (find-accounts "Savings" "Car" "Reserve")
          items (->> *context*
                     (filter :transaction/items)
                     (mapcat :transaction/items)
                     (filter #(or (id= reserve
                                       (:transaction-item/account %))
                                  (id= car
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

(def ^:private child-reconciliation-context
  (conj parent-account-context
        #:reconciliation{:account "Car"
                         :end-of-period (t/local-date 2015 1 31)
                         :balance 100M
                         :status :completed
                         :items [[(t/local-date 2015 1 1) 100M]]}))

(dbtest starting-balance-sums-each-childs-own-last-reconciliation-when-the-parent-has-none
  (with-context child-reconciliation-context
    (let [savings (find-account "Savings")]
      ; Savings itself has never been reconciled. Car has its own completed
      ; reconciliation (100M), but Reserve has never been reconciled, so it
      ; contributes nothing to the starting balance -- not its live ledger
      ; balance of 200M. Only amounts already confirmed via a reconciliation
      ; count.
      (assert-created
        #:reconciliation{:account savings
                         :end-of-period (t/local-date 2015 2 28)
                         :status :completed
                         :balance 100M}))))

(def ^:private absorbable-child-context
  (conj child-reconciliation-context
        #:reconciliation{:account "Reserve"
                         :end-of-period (t/local-date 2015 1 31)
                         :balance 200M
                         :status :completed
                         :items [[(t/local-date 2015 1 1) 200M]]}
        #:transaction{:transaction-date (t/local-date 2015 2 15)
                      :entity "Personal"
                      :description "More Car Funds"
                      :debit-account "Car"
                      :credit-account "Salary"
                      :quantity 30M}))

(dbtest starting-balance-excludes-a-descendant-once-its-items-are-absorbed-by-an-ancestor
  (with-context absorbable-child-context
    (let [savings (find-account "Savings")
          car (find-account "Car")
          new-car-item (find-transaction-item [(t/local-date 2015 2 15) 30M car])]
      ; Savings has no reconciliation of its own yet, so its first
      ; reconciliation sums Car's own (100M) and Reserve's own (200M): 300M,
      ; plus Car's new, not-yet-reconciled item (30M) = 330M.
      (assert-created
        #:reconciliation{:account savings
                         :end-of-period (t/local-date 2015 2 28)
                         :status :completed
                         :balance 330M
                         :items [new-car-item]})
      ; That reconciliation swept in one of Car's items, so Car's original
      ; 100M reconciliation is now considered absorbed and must not be added
      ; again. Reserve was never swept into any ancestor reconciliation, so
      ; its 200M still counts: 330 (Savings' own) + 200 (Reserve,
      ; un-absorbed) = 530M. Car contributes nothing further.
      (assert-created
        #:reconciliation{:account savings
                         :end-of-period (t/local-date 2015 3 31)
                         :status :completed
                         :balance 530M}))))

(def ^:private previous-balance-ctx
  (conj basic-context
        #:account{:type :liability
                  :entity "Personal"
                  :name "Credit Card"}
        #:account{:type :asset
                  :entity "Personal"
                  :name "Savings"}
        #:account{:type :asset
                  :entity "Personal"
                  :parent "Savings"
                  :name "Car"}
        #:account{:type :asset
                  :entity "Personal"
                  :parent "Savings"
                  :name "Reserve"}
        #:transaction{:transaction-date (t/local-date 2019 12 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :credit-account "Salary"
                      :debit-account "Checking"
                      :quantity 5000M}
        #:transaction{:transaction-date (t/local-date 2019 12 16)
                      :entity "Personal"
                      :description "Save for car"
                      :credit-account "Checking"
                      :debit-account "Car"
                      :quantity 300M}
        #:transaction{:transaction-date (t/local-date 2019 12 17)
                      :entity "Personal"
                      :description "Save for rainy day"
                      :credit-account "Checking"
                      :debit-account "Reserve"
                      :quantity 500M}
        #:transaction{:transaction-date (t/local-date 2020 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :credit-account "Salary"
                      :debit-account "Checking"
                      :quantity 5000M}
        #:transaction{:transaction-date (t/local-date 2020 1 2)
                      :entity "Personal"
                      :description "Landlord"
                      :credit-account "Checking"
                      :debit-account "Rent"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2020 1 3)
                      :entity "Personal"
                      :description "Kroger"
                      :credit-account "Credit Card"
                      :debit-account "Groceries"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2020 1 4)
                      :entity "Personal"
                      :description "Save for car"
                      :credit-account "Checking"
                      :debit-account "Car"
                      :quantity 300M}
        #:transaction{:transaction-date (t/local-date 2020 1 4)
                      :entity "Personal"
                      :description "Save for rainy day"
                      :credit-account "Checking"
                      :debit-account "Reserve"
                      :quantity 500M}
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2020 12 31)
                         :balance 5000M
                         :status :completed
                         :items [[(t/local-date 2019 12 15)
                                  5000M]]}
        #:reconciliation{:account "Car"
                         :end-of-period (t/local-date 2020 12 31)
                         :balance 300M
                         :status :completed
                         :items [[(t/local-date 2019 12 16)
                                  300M]]}
        #:reconciliation{:account "Reserve"
                         :end-of-period (t/local-date 2020 12 31)
                         :balance 500M
                         :status :completed
                         :items [[(t/local-date 2019 12 17)
                                  500M]]}))

(dbtest get-the-previous-reconciliation-balance-including-children
  (with-context previous-balance-ctx
    (testing "An account with no children"
      (testing "and no previous reconciliation"
        (is (= 0M (recs/previous-balance (find-account "Credit Card")
                                         :include-children? true))
            "The previous balance is zero"))
      (testing "and a previous reconciliation"
        (testing "that is completed"
          (is (= 5000M (recs/previous-balance (find-account "Checking")
                                              :include-children? true))
            "The previous balance comes from the most recent reconciliation"))
        (testing "that is not completed"
          ; A working (:new) reconciliation dated after the completed one
          ; must not contribute its own balance -- only the last *completed*
          ; reconciliation counts as history.
          (entities/put #:reconciliation{:account (find-account "Checking")
                                         :end-of-period (t/local-date 2021 1 31)
                                         :balance 4000M
                                         :status :new})
          (is (= 5000M (recs/previous-balance (find-account "Checking")
                                              :include-children? true))
              "The working reconciliation's balance is ignored; the last completed reconciliation still provides the previous balance"))))
    (testing "An account with children"
      (testing "and previous reconciliations at the child level"
        (is (= 800M (recs/previous-balance (find-account "Savings")
                                           :include-children? true))
            "The previous balance comes from the most recent reconciliation")
        (testing "and a reconciliation at the parent level that \"absorbs\" items from the child accounts."
          (let [savings (find-account "Savings")
                car-item (find-transaction-item [(t/local-date 2020 1 4) 300M (find-account "Car")])
                reserve-item (find-transaction-item [(t/local-date 2020 1 4) 500M (find-account "Reserve")])]
            ; Sweeping Car's and Reserve's newest, not-yet-reconciled items
            ; into a Savings-level reconciliation absorbs both children: their
            ; own 300M and 500M reconciliations must no longer be added on
            ; top of Savings' own balance (300 + 500 + 300 + 500 = 1600).
            (assert-created
              #:reconciliation{:account savings
                               :end-of-period (t/local-date 2021 2 28)
                               :status :completed
                               :balance 1600M
                               :items [car-item reserve-item]})
            (is (= 1600M (recs/previous-balance savings
                                                :include-children? true))
                "The parent's own reconciliation balance is used once the child accounts' items are absorbed into it")))))))

(dbtest get-the-previous-reconciliation-balance
  (with-context previous-balance-ctx
    (is (= 0M (recs/previous-balance (find-account "Savings")))
        "With include-children? false, the children's reconciliation balances are ignored, even though Savings itself has no reconciliation of its own")
    (testing "and a reconciliation at the parent level"
      (let [savings (find-account "Savings")
            car-item (find-transaction-item [(t/local-date 2020 1 4) 300M (find-account "Car")])
            reserve-item (find-transaction-item [(t/local-date 2020 1 4) 500M (find-account "Reserve")])]
        (assert-created
          #:reconciliation{:account savings
                           :end-of-period (t/local-date 2021 2 28)
                           :status :completed
                           :balance 1600M
                           :items [car-item reserve-item]})
        (is (= 1600M (recs/previous-balance savings))
            "Savings' own reconciliation balance is still used when it exists")))))

(def ^:private grandchild-context
  (conj parent-account-context
        #:account{:name "Reserve Sub"
                  :type :asset
                  :entity "Personal"
                  :parent "Reserve"}
        #:transaction{:transaction-date (t/local-date 2015 1 15)
                      :entity "Personal"
                      :description "Sub transfer"
                      :debit-account "Reserve Sub"
                      :credit-account "Reserve"
                      :quantity 50M}
        #:reconciliation{:account "Reserve Sub"
                         :end-of-period (t/local-date 2015 1 31)
                         :balance 50M
                         :status :completed
                         :items [[(t/local-date 2015 1 15) 50M]]}
        #:transaction{:transaction-date (t/local-date 2015 2 15)
                      :entity "Personal"
                      :description "More sub funds"
                      :debit-account "Reserve Sub"
                      :credit-account "Salary"
                      :quantity 10M}))

(dbtest previous-balance-includes-a-grandchilds-completed-balance
  (with-context grandchild-context
    ; Reserve Sub (a grandchild of Savings, child of Reserve) has its own
    ; completed reconciliation; neither Savings nor Reserve has one. The
    ; grandchild's balance must still be picked up.
    (is (= 50M (recs/previous-balance (find-account "Savings")
                                      :include-children? true)))))

(dbtest previous-balance-excludes-a-grandchild-once-absorbed-by-a-higher-ancestor
  (with-context grandchild-context
    (let [savings (find-account "Savings")
          reserve-sub (find-account "Reserve Sub")
          new-sub-item (find-transaction-item [(t/local-date 2015 2 15) 10M reserve-sub])]
      ; Savings' first reconciliation sums Reserve Sub's own completed
      ; balance (50M, not yet absorbed) plus its new, not-yet-reconciled item
      ; (10M): 60M.
      (assert-created
        #:reconciliation{:account savings
                         :end-of-period (t/local-date 2015 2 28)
                         :status :completed
                         :balance 60M
                         :items [new-sub-item]})
      ; Reserve Sub's own 50M reconciliation is now absorbed into Savings'
      ; reconciliation two levels up (skipping Reserve, which has no
      ; reconciliation of its own); it must not be counted again -- only
      ; Savings' own new balance (60M) counts going forward.
      (is (= 60M (recs/previous-balance savings :include-children? true))))))

(dbtest transaction-item-can-only-belong-to-one-reconciliation
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
  (with-context working-recon-context
    (let [result (-> (find-reconciliation ["Checking"
                                           (t/local-date 2017 1 3)])
                     entities/find ; get a fresh copy of the account items
                     (assoc :reconciliation/balance 1499M)
                     entities/put)]
      (is (comparable? {:reconciliation/balance 1499M}
                       result)
          "The result has the correct balance after update")
      (is (comparable? {:reconciliation/balance 1499M}
                       (entities/find result))
          "The retrieved value has the correct balance after update"))))

(dbtest a-working-reconciliation-can-be-completed
  (with-context working-recon-context
    (let [checking (find-account "Checking")
          previous-rec (find-reconciliation [checking (t/local-date 2017 1 1)])
          item (find-transaction-item [(t/local-date 2017 1 3)
                                   45M
                                   checking])
          result (-> (find-reconciliation [checking (t/local-date 2017 1 3)])
                     entities/find ; get a fresh copy of the account items
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
              :transaction-item/quantity 1000M
              :transaction-item/reconciliation (->entity-ref previous-rec)}
             {:transaction/transaction-date (t/local-date 2017 1 2)
              :transaction-item/quantity 500M
              :transaction-item/reconciliation (->entity-ref result)}
             {:transaction/transaction-date (t/local-date 2017 1 3)
              :transaction-item/quantity 45M
              :transaction-item/reconciliation (->entity-ref result)}
             {:transaction/transaction-date (t/local-date 2017 1 10)
              :transaction-item/quantity 53M
              :transaction-item/reconciliation nil}]
            (map #(update-in %
                             [:transaction-item/reconciliation]
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
  (with-context working-recon-context
    (let [item (find-transaction-item [(t/local-date 2017 1 10)
                                   53M
                                   "Checking"])]
      (-> (find-reconciliation ["Checking" (t/local-date 2017 1 3)])
          entities/find ; get a fresh copy of the items
          (assoc :reconciliation/status :completed)
          (update-in [:reconciliation/items]
                     conj
                     item)
          (assert-invalid {:reconciliation/balance ["Balance must match the calculated balance"]})))))

(dbtest an-item-can-be-removed-from-a-working-reconciliation
  (with-context working-recon-context
    (let [checking (find-account "Checking")
          item (find-transaction-item [(t/local-date 2017 1 2)
                                   500M
                                   checking])
          result (-> (find-reconciliation [checking (t/local-date 2017 1 3)])
                     entities/find ; get a fresh copy of the account items
                     (assoc :reconciliation/items []
                            :reconciliation/balance 1000M)
                     entities/put)]
      (is (comparable? #:reconciliation{:balance 1000M
                                        :status :new}
                       result)
          "The result reflects the updated balance")
      (is (comparable? {:transaction-item/reconciliation nil}
                       (update-in (entities/find item)
                                  [:transaction-item/reconciliation]
                                  identity))
          "The previously reconciled item is no longer linked to the reconciliation")
      (is (empty? (-> result entities/find :reconciliation/items))
          "The retrieved reconciliation no longer includes the removed item"))))

(dbtest an-unadjusted-balance-fails-after-removing-an-item
  (with-context working-recon-context
    (let [checking (find-account "Checking")]
      (-> (find-reconciliation [checking (t/local-date 2017 1 3)])
          entities/find ; get a fresh copy of the account items
          (assoc :reconciliation/items []
                 :reconciliation/status :completed)
          (assert-invalid {:reconciliation/balance ["Balance must match the calculated balance"]})))))

(dbtest a-completed-reconciliation-cannot-be-updated
  (with-context existing-reconciliation-context
    (-> (find-reconciliation ["Checking" (t/local-date 2017 1 1)])
        entities/find
        (assoc :reconciliation/end-of-period (t/local-date 2017 1 31))
        (assert-invalid {:reconciliation/status ["A completed reconciliation cannot be updated"]}))))

(dbtest the-most-recent-completed-reconciliation-can-be-deleted
  (with-context existing-reconciliation-context
    (assert-deleted (find-reconciliation ["Checking" (t/local-date 2017 1 1)]))))

(dbtest a-working-reconciliation-can-be-deleted
  (with-context working-recon-context
    (assert-deleted (find-reconciliation ["Checking" (t/local-date 2017 1 3)]))))

(dbtest ^:multi-threaded propagate-reconciliation-deletion
  (with-context working-recon-context
    (let [recon (entities/find
                  (find-reconciliation
                    ["Checking" (t/local-date 2017 1 3)]))]
      (prop/delete-and-propagate recon)
      (is (empty? (entities/select
                    {:transaction-item/reconciliation recon}))
          "The reconciliation is not associated with any items after delete"))))

(dbtest a-reconciliation-that-is-not-the-most-recent-cannot-be-deleted
  (with-context working-recon-context
    (let [recon (entities/find
                  (find-reconciliation
                    ["Checking" (t/local-date 2017 1 1)]))]
      (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
                            (entities/delete recon))
          "an exception is thrown")
      (is (entities/find recon)
          "The reconciliation can still be retrieved"))))

(dbtest ^:multi-threaded a-failed-attempt-to-delete-does-not-propagate
  (with-context working-recon-context
    (let [recon (entities/find
                  (find-reconciliation
                    ["Checking" (t/local-date 2017 1 1)]))]
      (is (thrown-with-msg? Exception #"Only the most recent reconciliation may be deleted"
                            (prop/delete-and-propagate recon))
          "an exception is thrown during propagation")
      (is (seq (entities/select
                 {:transaction-item/reconciliation recon}))
          "The transaction items are still associated with the reconciliation"))))
