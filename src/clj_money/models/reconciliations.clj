(ns clj-money.models.reconciliations
  (:require [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.storage :refer [create-reconciliation
                                              select-reconciliations-by-account-id]])
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

(defn- validate
  [reconciliation]
  (validation/validate ::new-reconciliation reconciliation))

(defn create
  "Creates a new reconciliation record"
  [storage-spec reconciliation]
  (with-transacted-storage [s storage-spec]
    (let [validated (->> reconciliation
                         before-validation
                         validate)]
      (if (validation/valid? validated)
        (->> validated
             before-save
             (create-reconciliation s ))
        validated))))

(defn find-by-account-id
  "Returns the reconciliations for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (map after-read
         (select-reconciliations-by-account-id s account-id))))
