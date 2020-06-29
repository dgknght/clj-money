(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core :as storage :refer [with-storage
                                               with-transacted-storage]]
            [clj-money.validation :as v :refer [with-validation]]
            [clj-money.x-platform.accounts :refer [->criteria]]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(s/def ::account-id integer?)
(s/def ::end-of-period v/local-date?)
(s/def ::balance decimal?)
(s/def ::status #{:new :completed})
(s/def ::item-ref (s/tuple uuid? v/local-date?))
(s/def ::item-refs (s/coll-of ::item-ref))

(s/def ::new-reconciliation (s/keys :req-un [::account-id ::end-of-period ::status ::balance] :opt-un [::item-refs]))
(s/def ::existing-reconciliation (s/keys :req-un [::id ::end-of-period ::status ::balance] :opt-un [::account-id ::item-refs]))

(defn- resolve-item-refs
  [item-refs storage]
  (if (seq item-refs)
    (let [ids (map first item-refs)
          date-range ((juxt first last) (sort (map second item-refs)))]
      (transactions/find-items-by-ids
        storage
        ids
        date-range))
    []))

(defn- fetch-items
  [{:keys [id account-id]} storage]
  (if id
    (let [accounts (accounts/search storage
                                    {:id account-id}
                                    {:include-children? true})
          criteria (assoc (->criteria accounts)
                          :reconciliation-id id)]
      (transactions/search-items storage criteria))
    []))

(defn- before-validation
  [{:keys [item-refs] :as reconciliation} storage]
  (let [existing-items (fetch-items reconciliation storage)
        ignore (->> existing-items
                    (map :id)
                    set)
        all-items (resolve-item-refs item-refs storage)]
    (-> reconciliation
        (update-in [:status] (fnil identity :new))
        (assoc ::new-items (->> all-items
                                (remove #(ignore (:id %)))
                                (into [])))
        (assoc ::all-items all-items)
        (assoc ::existing-items existing-items))))

(defn- before-save
  [reconciliation]
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

(defn find-by-id
  "Returns the specified reconciliation"
  [storage-spec id]
  (find storage-spec {:id id}))

(defn- is-in-balance?
  [storage {:keys [account-id balance] :as reconciliation}]
  (or (= :new (:status reconciliation))
      (let [starting-balance (or (:balance (find-last-completed
                                             storage
                                             account-id))
                                 0M)
            new-balance (->> (::all-items reconciliation)
                             (map :polarized-quantity)
                             (reduce + starting-balance))]
        (= balance new-balance))))

(defn- items-belong-to-account?
  [{:keys [account-id] :as reconciliation} storage]
  (or (empty? (::new-items reconciliation))
      (let [account-ids (->> (accounts/search
                               storage
                               {:id account-id}
                               {:include-children? true})
                             (map :id)
                             set)]
        (->> (::new-items reconciliation)
             (map :account-id)
             (every? #(account-ids %))))))

(defn- items-do-not-belong-to-another-reconciliation?
  [{:keys [id] :as reconciliation}]
  (let [reconciliation-ids (->> (::new-items reconciliation)
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
  [(v/create-rule (partial is-in-balance? storage)
                  [:balance]
                  "The account balance must match the statement balance.")
   (v/create-rule #(items-belong-to-account? % storage)
                  [:item-refs]
                  "All items must belong to the account being reconciled")
   (v/create-rule items-do-not-belong-to-another-reconciliation?
                  [:item-refs]
                  "No items may belong to another reconcilidation")
   (v/create-rule (partial is-after-last-reconciliation? storage)
                  [:end-of-period]
                  "End of period must be after the latest reconciliation")
   (v/create-rule (partial can-be-updated? storage)
                  [:status]
                  "A completed reconciliation cannot be updated")
   (v/create-rule (partial no-working-reconciliation-exists? storage)
                  [:account-id]
                  "A new reconciliation cannot be created while a working reconciliation already exists")])

(defn- ->date-range
  [items date-fn]
  (when (seq items)
    ((juxt first last) (->> items
                            (map date-fn)
                            sort))))

(defn- unreconcile-old-items
  [{:keys [id] :as reconciliation} storage]
  (when (seq (::existing-items reconciliation))
    (let [[start end] (->date-range (::existing-items reconciliation)
                                   :transaction-date)]
      (transactions/update-items storage
                                 {:reconciliation-id nil}
                                 {:reconciliation-id id
                                  :transaction-date [:between start end]}))))

(defn- reconcile-all-items
  [{id :id all-items ::all-items} storage]
  (when (seq all-items)
    (let [[start end] (->date-range all-items :transaction-date)]
      (transactions/update-items storage
                                 {:reconciliation-id id}
                                 {:id (map :id all-items)
                                  :transaction-date [:between start end]}))))

(defn- after-save
  [reconciliation storage]
  (unreconcile-old-items reconciliation storage)
  (reconcile-all-items reconciliation storage)
  reconciliation)

(defn reload
  "Returns the same reconciliation reloaded from the data store"
  [storage-spec {id :id}]
  (find-by-id storage-spec id))

(defn create
  [storage-spec reconciliation]
  (with-transacted-storage [s storage-spec]
    (let [recon (before-validation reconciliation s)]
      (with-validation recon ::new-reconciliation (validation-rules s)
        (let [to-create (before-save recon)
              created (storage/create s to-create)]
          (after-save (merge created
                             (select-keys to-create [:item-refs
                                                     ::all-items
                                                     ::existing-items
                                                     ::new-items]))
                      s)
          (reload s created))))))

(defn update
  [storage-spec reconciliation]
  (with-transacted-storage [s storage-spec]
    (let [recon (before-validation reconciliation s)]
      (with-validation recon ::existing-reconciliation (validation-rules s)
        (let [to-update (before-save recon)]
          (storage/update s to-update)
          (after-save to-update s)
          (reload s recon))))))

(defn delete
  "Removes the specified reconciliation from the system. (Only the most recent may be deleted.)"
  [storage-spec id]
  (with-transacted-storage [s storage-spec]
    (let [reconciliation (find-by-id s id)
          most-recent (find-last s (:account-id reconciliation))
          [start end] (->date-range (:item-refs reconciliation) second)]
      (when (not= id (:id most-recent))
        (throw (ex-info "Only the most recent reconciliation may be deleted" {:specified-reconciliation reconciliation
                                                                              :most-recent-reconciliation most-recent})))
      (when start
        (transactions/update-items s
                                   {:reconciliation-id nil}
                                   {:reconciliation-id id
                                    :transaction-date [:between start end]}))
      (storage/delete s reconciliation))))
