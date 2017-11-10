(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clojure.spec :as s]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.storage :refer [create-reconciliation
                                              update-reconciliation
                                              select-reconciliations-by-account-id
                                              select-transaction-items-by-reconciliation-id
                                              find-reconciliation-by-id
                                              find-last-reconciliation-by-account-id
                                              find-new-reconciliation-by-account-id
                                              set-transaction-items-reconciled
                                              unreconcile-transaction-items-by-reconciliation-id
                                              delete-reconciliation]])
  (:import org.joda.time.LocalDate))

(s/def ::account-id integer?)
(s/def ::end-of-period #(instance? LocalDate %))
(s/def ::balance decimal?)
(s/def ::status #{:new :completed})
(s/def ::item-id integer?)
(s/def ::item-ids (s/coll-of ::item-id))

(s/def ::new-reconciliation (s/keys :req-un [::account-id ::end-of-period ::status ::balance] :opt-un [::item-ids]))
(s/def ::existing-reconciliation (s/keys :req-un [::id ::end-of-period ::status ::balance] :opt-un [::account-id ::item-ids]))

(def ^:private coercion-rules
  [(coercion/rule :local-date [:end-of-period])
   (coercion/rule :decimal [:balance])
   (coercion/rule :integer [:account-id])
   (coercion/rule :integer [:id])
   (coercion/rule :integer-collection [:item-ids])])

(defn- before-validation
  [reconciliation]
  (-> (coercion/coerce coercion-rules reconciliation)
      (update-in [:status] (fnil identity :new))))

(defn- before-save
  [reconciliation]
  (-> reconciliation
      (update-in [:end-of-period] tc/to-long)
      (update-in [:status] name)))

(defn- after-read
  [reconciliation]
  (when reconciliation
    (-> reconciliation
        (update-in [:end-of-period] tc/to-local-date)
        (update-in [:status] keyword)
        (authorization/tag-resource :reconciliation))))

(defn find-last-completed
  "Returns the last reconciled balance for an account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (after-read (find-last-reconciliation-by-account-id s account-id :completed))))

; TODO this still isn't ensureing that they are only loaded once, need to rework it
(defn- ensure-transaction-items
  [storage {item-ids :item-ids :as reconciliation}]
  (update-in reconciliation
         [::items]
         (fnil identity (if item-ids
                          (transactions/find-items-by-ids storage item-ids)
                          []))))

(defn- append-transaction-item-ids
  [storage reconciliation]
  (when reconciliation
    (assoc reconciliation
           :item-ids
           (mapv :id (select-transaction-items-by-reconciliation-id
                       storage
                       (:id reconciliation))))))

(defn find-by-id
  "Returns the specified reconciliation"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> id
         (find-reconciliation-by-id s)
         (append-transaction-item-ids s)
         after-read)))

(defn- is-in-balance?
  [storage reconciliation]
  (or (= :new (:status reconciliation))
      (let [account (accounts/find-by-id storage (:account-id reconciliation))
            starting-balance (or (:balance (find-last-completed storage (:account-id reconciliation)))
                                 0M)
            delta (->> reconciliation
                       (ensure-transaction-items storage)
                       ::items
                       (map #(accounts/polarize-amount % account))
                       (reduce +))]
        (= (:balance reconciliation)
           (+ starting-balance delta)))))

(defn- items-belong-to-account?
  [storage {account-id :account-id :as reconciliation}]
  (or (nil? (:item-ids reconciliation))
      (= (->> reconciliation
              (ensure-transaction-items storage)
              ::items
              (map :account-id)
              set)
         #{account-id})))

(defn- items-do-not-belong-to-another-reconciliation?
  [storage {id :id :as reconciliation}]
  (let [reconciliation-ids (->> reconciliation
                                (ensure-transaction-items storage)
                                ::items
                                (map :reconciliation-id)
                                (filter identity)
                                set)]
    (or (empty? reconciliation-ids)
        (= reconciliation-ids #{id}))))

(defn- can-be-updated?
  [storage {:keys [status id]}]
  (or (nil? id)
      (= :new (:status (find-by-id storage id)))))

(defn- is-after-last-reconciliation?
  [storage reconciliation]
  (let [last-completed (find-last-completed storage (:account-id reconciliation))]
    (or (nil? last-completed)
        (> 0 (compare (:end-of-period last-completed)
                      (:end-of-period reconciliation))))))

(defn find-working
  "Returns the uncompleted reconciliation for the specified
  account, if one exists"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (when-let [reconciliation (find-new-reconciliation-by-account-id s account-id)]
      (after-read reconciliation))))

(defn- working-reconciliation-exists?
  [storage {:keys [account-id id] :as rec}]
  (when account-id
    (when-let [existing (find-working storage account-id)]
      (or (nil? id) (not= id (:id existing))))))

(defn- no-working-reconciliation-exists?
  [storage reconciliation]
  (not (working-reconciliation-exists? storage reconciliation)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial is-in-balance? storage)
                           [:balance]
                           "The account balance must match the statement balance.")
   (validation/create-rule (partial items-belong-to-account? storage)
                           [:item-ids]
                           "All items must belong to the account being reconciled")
   (validation/create-rule (partial items-do-not-belong-to-another-reconciliation? storage)
                           [:item-ids]
                           "No items may belong to another reconcilidation")
   (validation/create-rule (partial is-after-last-reconciliation? storage)
                           [:end-of-period]
                           "End of period must be after the latest reconciliation")
   (validation/create-rule (partial can-be-updated? storage)
                           [:status]
                           "A completed reconciliation cannot be updated")
   (validation/create-rule (partial no-working-reconciliation-exists? storage)
                           [:account-id]
                           "A new reconciliation cannot be created while a working reconciliation already exists")])

(defn- validate
  [spec rules reconciliation]
  (->> reconciliation
       before-validation
       (validation/validate spec rules)))

(defn create
  "Creates a new reconciliation record"
  [storage-spec reconciliation]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate ::new-reconciliation
                              (validation-rules s)
                              reconciliation)]
      (if (validation/valid? validated)
        (let [created (->> validated
                           before-save
                           (create-reconciliation s))]
          (when (and (:item-ids validated) (seq (:item-ids validated)))
            (set-transaction-items-reconciled s (:id created) (:item-ids validated)))
          (->> created
               (append-transaction-item-ids s)
               after-read))
        validated))))

(defn find-by-account-id
  "Returns the reconciliations for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (map after-read
         (select-reconciliations-by-account-id s account-id))))

(defn reload
  "Returns the same reconciliation reloaded from the data store"
  [storage-spec {id :id}]
  (find-by-id storage-spec id))

(defn- set-account-id
  [storage reconciliation]
  (let [existing (find-by-id storage (Integer. (:id reconciliation)))]
    (assoc reconciliation :account-id (:account-id existing))))

(defn update
  "Updates the specified reconciliation"
  [storage-spec reconciliation]
  (with-storage [s storage-spec]
    (let [validated (->> reconciliation
                         (set-account-id s)
                         (validate ::existing-reconciliation (validation-rules s)))]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save
               (update-reconciliation s))
          (unreconcile-transaction-items-by-reconciliation-id s (:id validated))
          (when (and (:item-ids validated) (seq (:item-ids validated)))
            (set-transaction-items-reconciled s (:id validated) (:item-ids validated)))
          (reload s validated))
        validated))))

(defn delete
  "Removes the specified reconciliation from the system. (Only the most recent may be deleted.)"
  [storage-spec id]
  (with-transacted-storage [s storage-spec]
    (let [reconciliation (find-by-id s id)
          most-recent (find-last-reconciliation-by-account-id
                        s
                        (:account-id reconciliation))]
      (when (not= id (:id most-recent))
        (throw (ex-info "Only the most recent reconciliation may be deleted" {:specified-reconciliation reconciliation
                                                                              :most-recent-reconciliation most-recent})))
      (unreconcile-transaction-items-by-reconciliation-id s id)
      (delete-reconciliation s id))))
