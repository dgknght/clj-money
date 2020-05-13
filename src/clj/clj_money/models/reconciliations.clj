(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core :as storage :refer [with-storage with-transacted-storage]]
            [clj-money.util :refer [rev-args]]
            [clj-money.validation :as validation]
            [clj-money.x-platform.accounts :refer [polarize-quantity]]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [create-fn
                                              update-fn]]))

(s/def ::account-id integer?)
(s/def ::end-of-period validation/local-date?)
(s/def ::balance decimal?)
(s/def ::status #{:new :completed})
(s/def ::item-ref (s/tuple uuid? validation/local-date?))
(s/def ::item-refs (s/coll-of ::item-ref))

(s/def ::new-reconciliation (s/keys :req-un [::account-id ::end-of-period ::status ::balance] :opt-un [::item-refs]))
(s/def ::existing-reconciliation (s/keys :req-un [::id ::end-of-period ::status ::balance] :opt-un [::account-id ::item-refs]))

(defn- before-validation
  [reconciliation & _]
  (update-in reconciliation [:status] (fnil identity :new)))

(defn- before-save
  [reconciliation & _]
  (-> reconciliation
      (storage/tag ::models/reconciliation)
      (update-in [:status] name)))

(defn- append-transaction-item-refs
  [reconciliation storage]
  (when reconciliation
    (assoc reconciliation
           :item-refs
           (mapv (juxt :id :transaction-date)
                 (transactions/select-items-by-reconciliation
                       storage
                       reconciliation)))))

(defn- after-read
  [reconciliation storage]
  (when reconciliation
    (-> reconciliation
        (update-in [:status] keyword)
        (storage/tag ::models/reconciliation)
        (append-transaction-item-refs storage))))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map #(after-read % s)
          (storage/select  s
                          (storage/tag criteria ::models/reconciliation)
                          options)))))

(defn find
  ([storage-spec criteria]
   (find storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (merge options {:limit 1})))))

(defn find-last
  "Returns the last reconciliation for an account"
  [storage-spec account-id]
  (find storage-spec
        {:account-id account-id}
        {:sort [[:end-of-period :desc]]}))

(defn find-last-completed
  "Returns the last completed reconciliation for an account"
  [storage-spec account-id]
  (find storage-spec
        {:account-id account-id
         :status "completed"}
        {:sort [[:end-of-period :desc]]}))

; TODO this still isn't ensureing that they are only loaded once, need to rework it
(defn- ensure-transaction-items
  [storage {item-refs :item-refs :as reconciliation}]
  (let [ids (map first item-refs)
        date-range ((juxt first last) (sort (map second item-refs)))]
    (update-in reconciliation
               [::items]
               (fnil identity (if item-refs
                                (transactions/find-items-by-ids
                                  storage
                                  ids
                                  date-range)
                                [])))))

(defn find-by-id
  "Returns the specified reconciliation"
  [storage-spec id]
  (find storage-spec {:id id}))

(defn- is-in-balance?
  [storage {:keys [account-id] :as reconciliation}]
  (or (= :new (:status reconciliation))
      (let [account (accounts/find-by-id storage account-id)
            starting-balance (or (:balance (find-last-completed
                                             storage
                                             account-id))
                                 0M)
            delta (->> reconciliation
                       (ensure-transaction-items storage)
                       ::items
                       (map #(polarize-quantity % account))
                       (reduce +))]
        (= (:balance reconciliation)
           (+ starting-balance delta)))))

(defn- items-belong-to-account?
  [storage {account-id :account-id :as reconciliation}]
  (or (nil? (:item-refs reconciliation))
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
  [storage {:keys [id]}]
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
  (find storage-spec
        {:status "new"
         :account-id account-id}))

(defn- working-reconciliation-exists?
  [storage {:keys [account-id id]}]
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
                           [:item-refs]
                           "All items must belong to the account being reconciled")
   (validation/create-rule (partial items-do-not-belong-to-another-reconciliation? storage)
                           [:item-refs]
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

(defn- item-refs->date-range
  [item-refs]
  (when (seq item-refs)
    ((juxt first last) (->> item-refs
                            (map second)
                            sort))))

(defn- unreconcile
  [reconciliation-id date-range storage]
  (transactions/update-items storage
                             {:reconciliation-id nil}
                             {:reconciliation-id reconciliation-id
                              :transaction-date date-range}))

(defn- reconcile
  [reconciliation-id [item-id trans-date] storage]
  (transactions/update-items storage
                             {:reconciliation-id reconciliation-id}
                             {:id item-id
                              :transaction-date trans-date}))

(defn- after-save
  [{:keys [id item-refs] :as reconciliation} storage]
  ; Set reconciled flag on specified transaction items
  (let [date-range (item-refs->date-range item-refs)]
    (when date-range
      (unreconcile id date-range storage))
    (when (seq item-refs)
      (doseq [item-ref item-refs]
        (reconcile id item-ref storage))))
  reconciliation)

(defn- create*
  [storage reconciliation]
  (merge reconciliation
  (storage/create storage reconciliation)))

(def create
  (create-fn {:spec ::new-reconciliation
              :before-validation before-validation
              :create (rev-args create*)
              :before-save before-save
              :after-save after-save
              :rules-fn validation-rules
              :after-read after-read}))

(defn reload
  "Returns the same reconciliation reloaded from the data store"
  [storage-spec {id :id}]
  (find-by-id storage-spec id))

(def update
  (update-fn {:spec ::existing-reconciliation
              :before-validation before-validation
              :update (rev-args storage/update)
              :before-save before-save
              :after-save after-save
              :rules-fn validation-rules
              :after-read after-read
              :reload reload}))

(defn delete
  "Removes the specified reconciliation from the system. (Only the most recent may be deleted.)"
  [storage-spec id]
  (with-transacted-storage [s storage-spec]
    (let [reconciliation (find-by-id s id)
          most-recent (find-last s (:account-id reconciliation))
          date-range (item-refs->date-range (:item-refs reconciliation))]
      (when (not= id (:id most-recent))
        (throw (ex-info "Only the most recent reconciliation may be deleted" {:specified-reconciliation reconciliation
                                                                              :most-recent-reconciliation most-recent})))
      (when date-range
        (unreconcile id date-range s))
      (storage/delete s reconciliation))))
