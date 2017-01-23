(ns clj-money.models.reconciliations
  (:require [clojure.spec :as s]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.storage :refer [create-reconciliation
                                              select-reconciliations-by-account-id
                                              find-reconciliation-by-id
                                              find-last-complete-reconciliation-by-account-id
                                              find-new-reconciliation-by-account-id
                                              set-transaction-items-reconciled]])
  (:import org.joda.time.LocalDate))

(s/def ::account-id integer?)
(s/def ::end-of-period #(instance? LocalDate %))
(s/def ::balance decimal?)
(s/def ::status #{:new :completed})
(s/def ::item-id integer?)
(s/def ::item-ids (s/coll-of ::item-id))

(s/def ::new-reconciliation (s/keys :req-un [::account-id ::end-of-period ::status ::balance] :opt-un [::item-ids]))

(def ^:private coercion-rules
  [(coercion/rule :local-date [:end-of-period])
   (coercion/rule :decimal [:balance])
   (coercion/rule :integer [:account-id])
   (coercion/rule :integer-collection [:item-ids])])

(defn- before-validation
  [reconciliation]
  (-> reconciliation
      (coercion/coerce coercion-rules)
      (update-in [:status] (fnil identity :new))))

(defn- before-save
  [reconciliation]
  (-> reconciliation
      (update-in [:end-of-period] tc/to-long)
      (update-in [:status] name)))

(defn- after-read
  [reconciliation]
  (-> reconciliation
      (update-in [:end-of-period] tc/to-local-date)
      (update-in [:status] keyword)))

(defn previous-balance
  "Returns the last reconciled balance for an account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (or  (->> account-id
              (find-last-complete-reconciliation-by-account-id s)
              :balance)
        0M)))

(defn- is-in-balance?
  [storage reconciliation]
  (or (= :new (:status reconciliation))
      (let [account (accounts/find-by-id storage (:account-id reconciliation))
            starting-balance (previous-balance storage (:account-id reconciliation))
            delta (->> (transactions/find-items-by-ids storage
                                                       (:item-ids reconciliation))
                       (map #(accounts/polarize-amount % account))
                       (reduce +))]
        (= (:balance reconciliation)
           (+ starting-balance delta)))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial is-in-balance? storage)
                           [:balance]
                           "The account balance must match the statement balance.")])

(defn- validate
  [rules reconciliation]
  (apply validation/validate
         ::new-reconciliation
         (before-validation reconciliation)
         rules))

(defn create
  "Creates a new reconciliation record"
  [storage-spec reconciliation]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate (validation-rules s) reconciliation)]
      (if (validation/valid? validated)
        (let [created (->> validated
                           before-save
                           (create-reconciliation s))]
          (when (and (:item-ids validated) (seq (:item-ids validated)))
            (set-transaction-items-reconciled s (:id created) (:item-ids validated)))
          (after-read created))
        validated))))

(defn find-by-account-id
  "Returns the reconciliations for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (map after-read
         (select-reconciliations-by-account-id s account-id))))

(defn find-by-id
  "Returns the specified reconciliation"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> id
         (find-reconciliation-by-id s)
         after-read)))

(defn find-working
  "Returns the uncompleted reconciliation for the specified
  account, if one exists"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (when-let [reconciliation (find-new-reconciliation-by-account-id s account-id)]
      (after-read reconciliation))))
