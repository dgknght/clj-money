(ns clj-money.models.scheduled-transactions-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.models :as models]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-deleted]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account
                                            find-scheduled-transaction
                                            basic-context]]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:scheduled-transaction{:entity (find-entity "Personal")
                          :start-date (t/local-date 2021 3 31)
                          :end-date (t/local-date 2022 12 31)
                          :enabled true
                          :date-spec {:day :last}
                          :interval-type :month
                          :interval-count 1
                          :description "Paycheck"
                          :memo "scheduled"
                          :items [#:scheduled-transaction-item{:action :debit
                                                               :account (find-account "Checking")
                                                               :quantity 1000M}
                                  #:scheduled-transaction-item{:action :credit
                                                               :account (find-account "Salary")
                                                               :quantity 1000M}]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:scheduled-transaction/entity
                                      [:scheduled-transaction/items :scheduled-transaction-item/account]]))

(deftest create-a-scheduled-transaction
  (with-context
    (assert-created (attributes))))

(deftest entity-id-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/entity)
                    {:scheduled-transaction/entity ["Entity is required"]})))

(deftest description-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/description)
                    {:scheduled-transaction/description ["Description is required"]})))

(deftest interval-type-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/interval-type)
                    {:scheduled-transaction/interval-type ["Interval type is required"]})))

(deftest interval-type-can-be-week
  (with-context
    (assert-created (assoc (attributes) :scheduled-transaction/interval-type :week))))

(deftest interval-type-can-be-month
  (with-context
    (assert-created (assoc (attributes) :scheduled-transaction/interval-type :month))))

(deftest interval-type-can-be-year
  (with-context
    (assert-created (assoc (attributes) :scheduled-transaction/interval-type :year))))

(deftest interval-type-cannot-be-off-list
  (with-context
    (assert-invalid
      (assoc (attributes) :scheduled-transaction/interval-type :not-valid)
      {:scheduled-transaction/interval-type ["Interval type must be day, week, month, or year"]})))

(deftest start-date-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/start-date)
                    {:scheduled-transaction/start-date ["Start date is required"]})))

(deftest date-spec-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/date-spec)
                    {:scheduled-transaction/date-spec ["Date spec is required"]})))

(deftest interval-count-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :scheduled-transaction/interval-count)
                    {:scheduled-transaction/interval-count ["Interval count is required"]})))

(deftest interval-must-be-greater-than-zero
  (with-context
    (assert-invalid
      (assoc (attributes) :scheduled-transaction/interval-count 0)
      {:scheduled-transaction/interval-count ["Interval count must be greater than zero"]})))

(deftest at-least-two-items-are-required
  (with-context
    (assert-invalid
      (update-in (attributes) [:scheduled-transaction/items] #(take 1 %))
      {:scheduled-transaction/items ["Items must contain at least 2 item(s)"]})))

(deftest sum-of-credits-must-equal-sum-of-debits
  (with-context
    (assert-invalid
      (assoc-in (attributes)
                [:scheduled-transaction/items
                 0
                 :scheduled-transaction-item/quantity]
                1001M)
      {:scheduled-transaction/items ["The sum of debits must equal the sum of credits"]})))

(deftest item-account-id-is-required
  (with-context
    (assert-invalid
      (update-in (attributes)
                 [:scheduled-transaction/items 0]
                 dissoc
                 :scheduled-transaction-item/account)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/account
         ["Account is required"]}}})))

(deftest item-action-is-required
  (with-context
    (assert-invalid
      (update-in (attributes)
                 [:scheduled-transaction/items 0]
                 dissoc
                 :scheduled-transaction-item/action)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/action
         ["Action is required"]}}})))

(deftest item-action-must-be-credit-or-debit
  (with-context
    (assert-invalid
      (assoc-in
        (attributes)
        [:scheduled-transaction/items
         0
         :scheduled-transaction-item/action]
        :not-valid)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/action
         ["Action is invalid"]}}})))

(deftest item-quantity-is-required
  (with-context
    (assert-invalid
      (update-in (attributes)
                 [:scheduled-transaction/items 0]
                 dissoc
                 :scheduled-transaction-item/quantity)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/quantity
         ["Quantity is required"]}}})))

(deftest item-quantity-must-be-greater-than-zero
  (with-context
    (assert-invalid
      (assoc-in
        (attributes)
        [:scheduled-transaction/items
         0
         :scheduled-transaction-item/quantity]
        0M)
      {:scheduled-transaction/items
       {0
        {:scheduled-transaction-item/quantity
         ["Quantity must be greater than zero"]}}})))

(def ^:private update-context
  (conj basic-context
        #:scheduled-transaction{:entity "Personal"
                                :description "Paycheck"
                                :start-date (t/local-date 2016 1 1)
                                :date-spec {:day 1}
                                :interval-type :month
                                :interval-count 1
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Checking"
                                                                     :quantity 900M}
                                        #:scheduled-transaction-item{:action :debit
                                                                     :account "FIT"
                                                                     :quantity 100M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Salary"
                                                                     :quantity 1000M}]}))

(deftest update-a-scheduled-transaction
  (with-context update-context
    (let [attrs #:scheduled-transaction{:interval-type :week
                                        :interval-count 2}
          trx (find-scheduled-transaction "Paycheck")]
      (is (comparable? attrs
                       (models/put
                         (-> trx
                             (merge attrs)
                             (assoc-in [:scheduled-transaction/items
                                        0
                                        :scheduled-transaction-item/quantity]
                                       901M)
                             (assoc-in [:scheduled-transaction/items
                                        2
                                        :scheduled-transaction-item/quantity]
                                       1001M))))
          "The return value has the updated attributes")
      (let [{:as retrieved :scheduled-transaction/keys [items]} (models/find trx)]
        (is (comparable? attrs retrieved)
            "The retrieved value has the updated attributes")
        (is (seq-of-maps-like? [#:scheduled-transaction-item{:quantity 901M}
                                #:scheduled-transaction-item{:quantity 100M}
                                #:scheduled-transaction-item{:quantity 1001M}]
                               items)
            "The retrieved value has the updated items")))))

(deftest add-an-item
  (with-context update-context
    (let [trx (find-scheduled-transaction "Paycheck")
          result (models/put
                   (-> trx
                       (assoc-in [:scheduled-transaction/items
                                  0
                                  :scheduled-transaction-item/quantity]
                                 850M)
                       (update-in [:scheduled-transaction/items]
                                  conj
                                  #:scheduled-transaction-item{:action :debit
                                                               :account (find-account "Medicare")
                                                               :quantity 50M})
                       models/put))]
      (is (seq-of-maps-like? [#:scheduled-transaction-item{:quantity 850M}
                              #:scheduled-transaction-item{:quantity 100M}
                              #:scheduled-transaction-item{:quantity 1000M}
                              #:scheduled-transaction-item{:quantity 50M}]
                             (:scheduled-transaction/items result))
          "The returned value has the updated items")
      (is (seq-of-maps-like? [#:scheduled-transaction-item{:quantity 850M}
                              #:scheduled-transaction-item{:quantity 100M}
                              #:scheduled-transaction-item{:quantity 1000M}
                              #:scheduled-transaction-item{:quantity 50M}]
                             (:scheduled-transaction/items (models/find result)))
          "The returned value has the updated items"))))

(deftest delete-a-scheduled-transaction
  (with-context update-context
    (assert-deleted (find-scheduled-transaction "Paycheck"))))
