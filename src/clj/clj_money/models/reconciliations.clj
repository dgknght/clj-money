(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [clj-money.util :refer [->id]]
            [clj-money.validation :as v :refer [with-validation]]
            [clj-money.accounts :refer [->criteria]]
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
  [item-refs]
  (if (seq item-refs)
    (let [ids (map first item-refs)
          date-range ((juxt first last) (sort (map second item-refs)))]
      (transactions/find-items-by-ids
       ids
       date-range))
    []))

(defn- fetch-items
  [{:keys [id account-id]}]
  (if id
    (let [accounts (accounts/search {:id account-id}
                                    {:include-children? true})
          criteria (assoc (->criteria accounts)
                          :reconciliation-id id)]
      (transactions/search-items criteria))
    []))

(defn- before-validation
  [{:keys [item-refs] :as reconciliation}]
  (let [existing-items (fetch-items reconciliation)
        ignore (->> existing-items
                    (map :id)
                    set)
        new-items (->> item-refs
                       (remove #(ignore (first %)))
                       resolve-item-refs
                       (into []))
        all-items (concat existing-items new-items)]
    (-> reconciliation
        (update-in [:status] (fnil identity :new))
        (assoc ::new-items new-items
               ::all-items all-items
               ::existing-items existing-items))))

(defn- before-save
  [reconciliation]
  (-> reconciliation
      (tag ::models/reconciliation)
      (update-in [:status] name)))

(defn- append-transaction-item-refs
  [reconciliation]
  (when reconciliation
    (assoc reconciliation
           :item-refs
           (mapv (juxt :id :transaction-date)
                 (transactions/select-items-by-reconciliation
                  reconciliation)))))

(defn- after-read
  [reconciliation]
  (when reconciliation
    (-> reconciliation
        (update-in [:status] keyword)
        (tag ::models/reconciliation)
        (append-transaction-item-refs))))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/reconciliation)
                          options)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  "Returns the specified reconciliation"
  [reconciliation-or-id]
  (find-by {:id (->id reconciliation-or-id)}))

(defn find-last
  "Returns the last reconciliation for an account"
  [account-id]
  (find-by {:account-id account-id}
           {:sort [[:end-of-period :desc]]}))

(defn find-last-completed
  "Returns the last completed reconciliation for an account"
  [account-id]
  (find-by {:account-id account-id
            :status "completed"}
           {:sort [[:end-of-period :desc]]}))

(defn- is-in-balance?
  [{:keys [account-id balance] :as reconciliation}]
  (or (= :new (:status reconciliation))
      (let [starting-balance (or (:balance (find-last-completed
                                            account-id))
                                 0M)
            new-balance (->> (::all-items reconciliation)
                             (map :polarized-quantity)
                             (reduce + starting-balance))]
        (= balance new-balance))))

(defn- items-belong-to-account?
  [{:keys [account-id] :as reconciliation}]
  (or (empty? (::new-items reconciliation))
      (let [account-ids (->> (accounts/search
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
  [{:keys [id]}]
  (or (nil? id)
      (= :new (:status (find id)))))

(defn- is-after-last-reconciliation?
  [reconciliation]
  (let [last-completed (find-last-completed (:account-id reconciliation))]
    (or (nil? last-completed)
        (> 0 (compare (:end-of-period last-completed)
                      (:end-of-period reconciliation))))))

(defn find-working
  "Returns the uncompleted reconciliation for the specified
  account, if one exists"
  [account-id]
  (find-by {:status "new"
            :account-id account-id}))

(defn- working-reconciliation-exists?
  [{:keys [account-id id]}]
  (when account-id
    (when-let [existing (find-working account-id)]
      (or (nil? id) (not= id (:id existing))))))

(def ^:private validation-rules
  [(v/create-rule is-in-balance?
                  [:balance]
                  "The account balance must match the statement balance.")
   (v/create-rule items-belong-to-account?
                  [:item-refs]
                  "All items must belong to the account being reconciled")
   (v/create-rule items-do-not-belong-to-another-reconciliation?
                  [:item-refs]
                  "No items may belong to another reconcilidation")
   (v/create-rule is-after-last-reconciliation?
                  [:end-of-period]
                  "End of period must be after the latest reconciliation")
   (v/create-rule can-be-updated?
                  [:status]
                  "A completed reconciliation cannot be updated")
   (v/create-rule (complement working-reconciliation-exists?)
                  [:account-id]
                  "A new reconciliation cannot be created while a working reconciliation already exists")])

(defn- ->date-range
  [items date-fn]
  (when (seq items)
    ((juxt first last) (->> items
                            (map date-fn)
                            sort))))

(defn- unreconcile-old-items
  [{:keys [id] :as reconciliation}]
  (when (seq (::existing-items reconciliation))
    (let [[start end] (->date-range (::existing-items reconciliation)
                                    :transaction-date)]
      (transactions/update-items {:reconciliation-id nil}
                                 {:reconciliation-id id
                                  :transaction-date [:between start end]}))))

(defn- reconcile-all-items
  [{id :id all-items ::all-items}]
  (when (seq all-items)
    (let [[start end] (->date-range all-items :transaction-date)]
      (transactions/update-items {:reconciliation-id id}
                                 {:id (map :id all-items)
                                  :transaction-date [:between start end]}))))

(defn- after-save
  [reconciliation]
  (unreconcile-old-items reconciliation)
  (reconcile-all-items reconciliation)
  reconciliation)

(defn reload
  "Returns the same reconciliation reloaded from the data store"
  [reconciliation]
  (find reconciliation))

(defn create
  [reconciliation]
  (with-transacted-storage (env :db)
    (let [recon (before-validation reconciliation)]
      (with-validation recon ::new-reconciliation validation-rules
        (let [to-create (before-save recon)
              created (storage/create to-create)]
          (-> (merge created
                     (select-keys to-create [:item-refs
                                             ::all-items
                                             ::existing-items
                                             ::new-items]))
              after-save
              after-read))))))

(defn update
  [recon]
  (with-transacted-storage (env :db)
    (let [recon (before-validation recon)]
      (with-validation recon ::existing-reconciliation validation-rules
        (let [to-update (before-save recon)]
          (storage/update to-update)
          (after-save to-update)
          (reload recon))))))

(defn delete
  "Removes the specified reconciliation from the system. (Only the most recent may be deleted.)"
  [{:keys [account-id item-refs id] :as recon}]
  (with-transacted-storage (env :db)
    (let [most-recent (find-last account-id)
          [start end] (->date-range item-refs second)]
      (when (not= id (:id most-recent))
        (throw (ex-info "Only the most recent reconciliation may be deleted"
                        {:specified-reconciliation recon
                         :most-recent-reconciliation most-recent})))
      (when start
        (transactions/update-items {:reconciliation-id nil}
                                   {:reconciliation-id id
                                    :transaction-date [:between start end]}))
      (storage/delete recon))))
