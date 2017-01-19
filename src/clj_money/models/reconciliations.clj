(ns clj-money.models.reconciliations
  (:require [clojure.spec :as s]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.storage :refer [create-reconciliation
                                              select-reconciliations-by-account-id
                                              set-transaction-items-reconciled]])
  (:import org.joda.time.LocalDate))

(s/def ::account-id integer?)
(s/def ::end-of-period #(instance? LocalDate %))
(s/def ::status #{:new :completed})

(s/def ::new-reconciliation (s/keys :req-un [::account-id ::end-of-period ::status]))

(defn- before-validation
  [reconciliation]
  (update-in reconciliation [:status] (fnil identity :new)))

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

(defn- is-in-balance?
  [storage reconciliation]
  (or (= :new (:status reconciliation))
      (let [account (accounts/find-by-id storage (:account-id reconciliation))
            starting-balance (or  (->> (:account-id reconciliation)
                                       (select-reconciliations-by-account-id storage)
                                       (sort-by :end-of-balance)
                                       last
                                       :balance)
                                 0M)
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
          (set-transaction-items-reconciled s (:id created) (:item-ids validated))
          (after-read created))
        validated))))

(defn find-by-account-id
  "Returns the reconciliations for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (map after-read
         (select-reconciliations-by-account-id s account-id))))
