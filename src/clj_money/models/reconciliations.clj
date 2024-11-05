(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [config.core :refer [env]]
            [java-time.api :as t]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.accounts :refer [->criteria]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(declare find
         find-by
         find-last-completed)

(defn- in-balance?
  [{:reconciliation/keys [balance] :as reconciliation}]
  (let [starting-balance (get-in reconciliation
                                 [::last-completed :reconciliation/balance]
                                 0M)
        new-balance (->> (::all-items reconciliation)
                         (map :transaction-item/polarized-quantity)
                         (reduce + starting-balance))]
    (= balance new-balance)))

(defn- in-progress?
  [{:reconciliation/keys [status]}]
  (not= :completed status))

(def not-unbalanced?
  (some-fn in-progress? in-balance?))

(v/reg-spec not-unbalanced?
            {:message "%s must match the calculated balance"
             :path [:reconciliation/balance]})

(defn find-working
  "Returns the uncompleted reconciliation for the specified
  account, if one exists"
  [account]
  (models/find-by #:reconciliation{:status :new
                                   :account account}))

(defn- working-reconciliation-exists?
  [{:keys [account-id id]}]
  (when-let [existing (when account-id
                        (find-working account-id))]
    (or (nil? id)
        (not= id (:id existing)))))

(def no-working-conflict?
  (complement working-reconciliation-exists?))

(v/reg-spec no-working-conflict?
            {:message "%s already has a reconciliation in progress"
             :path [:reconciliation/account]})

(defn- items-belong-to-account?
  [{:reconciliation/keys [account] :as reconciliation}]
  (or (empty? (::new-items reconciliation))
      (let [account-ids (->> (models/select
                               (db/model-type
                                 {:id (:id account)}
                                 :account)
                               {:include-children? true})
                             (map :id)
                             set)]
        (->> (::new-items reconciliation)
             (map (comp :id
                        :reconciliation/account))
             (every? #(account-ids %))))))

(v/reg-spec items-belong-to-account?
            {:message "All items must belong to the account being reconciled"
             :path [:reconciliation/item-refs]})

(defn- items-not-already-reconciled?
  [{:keys [id] :as reconciliation}]
  (->> (::new-items reconciliation)
       (map (comp :id :transaction-item/reconciliation))
       (remove (some-fn nil? #(= id %)))
       empty?))

(v/reg-spec items-not-already-reconciled? {:message "No item can belong to another reconciliation"
                                           :path [:reconciliation/item-refs]})

(defn- can-be-updated?
  [recon]
  (or (nil? (:id recon))
      (= :new (:reconciliation/status (models/find recon)))))

(v/reg-spec can-be-updated?
            {:message "A completed reconciliation cannot be updated"
             :path [:reconciliation/status]})

(defn- after-last-reconciliation?
  [{::keys [last-completed] :as reconciliation}]
  (or (nil? last-completed)
      (t/before? (:reconciliation/end-of-period last-completed)
                 (:reconciliation/end-of-period reconciliation))))

(v/reg-spec after-last-reconciliation?
            {:message "%s must be after that latest reconciliation"
             :path [:reconciliation/end-of-period]})

(s/def :reconciliation/account util/model-ref?)
(s/def :reconciliation/end-of-period t/local-date?)
(s/def :reconciliation/balance decimal?)
(s/def :reconciliation/status #{:new :completed})
(s/def :reconciliation/item-ref (s/tuple uuid? t/local-date?))
(s/def :reconciliation/item-refs (s/coll-of :reconciliation/item-ref))

(s/def ::models/reconciliation (s/and (s/keys :req [:reconciliation/account
                                                    :reconciliation/end-of-period
                                                    :reconciliation/status
                                                    :reconciliation/balance]
                                              :opt [:reconciliation/item-refs])
                                      not-unbalanced?
                                      no-working-conflict?
                                      items-belong-to-account?
                                      items-not-already-reconciled?
                                      can-be-updated?
                                      after-last-reconciliation?))

(defn- resolve-item-refs
  [item-refs]
  (if (seq item-refs)
    (let [ids (map first item-refs)
          [start end] ((juxt first last) (sort (map second item-refs)))]
      (models/select {:id [:in ids]
                      :transaction-date [:between start end]}))
    []))

(defn- fetch-items
  [{:keys [id account-id]}]
  #_(if id
    (let [accounts (accounts/search {:id account-id}
                                    {:include-children? true})
          criteria (assoc (->criteria accounts)
                          :reconciliation-id id)]
      (transactions/search-items criteria))
    []))

(defn- find-last-completed
  "Returns the last completed reconciliation for an account"
  [{:keys [account-id id]}]
  (when account-id
    (find-by (assoc-if  {:account-id account-id
                         :status "completed"}
                       :id (when id [:!= id]))
             {:sort [[:end-of-period :desc]]})))

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
               ::existing-items existing-items
               ::last-completed (find-last-completed reconciliation)))))

(defn- before-save
  [reconciliation]
  (-> reconciliation
      (tag ::models/reconciliation)
      (update-in [:status] name)))

(defn- append-transaction-item-refs
  [reconciliation]
  reconciliation
  #_(when reconciliation
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

(defn- ->date-range
  [items date-fn]
  (when (seq items)
    ((juxt first last) (->> items
                            (map date-fn)
                            sort))))

(defn- unreconcile-old-items
  [{:keys [id] :as reconciliation}]
  #_(when (seq (::existing-items reconciliation))
    (let [[start end] (->date-range (::existing-items reconciliation)
                                    :transaction-date)]
      (transactions/update-items {:reconciliation-id nil}
                                 {:reconciliation-id id
                                  :transaction-date [:between start end]}))))

(defn- reconcile-all-items
  [{id :id all-items ::all-items}]
  #_(when (seq all-items)
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
      (with-validation recon ::reconciliation
        (let [to-create (before-save recon)
              created (storage/create to-create)]
          (-> (merge created
                     (select-keys to-create [:item-refs
                                             ::all-items
                                             ::existing-items
                                             ::new-items]))
              after-save
              after-read))))))

(defn ^:deprecated update
  [recon]
  (throw (UnsupportedOperationException. "reconciliations/update is deprecated"))
  #_(with-transacted-storage (env :db)
    (let [recon (before-validation recon)]
      (with-validation recon ::reconciliation
        (let [to-update (before-save recon)]
          (storage/update to-update)
          (after-save to-update)
          (reload recon))))))

(defn ^:deprecated delete
  "Removes the specified reconciliation from the system. (Only the most recent may be deleted.)"
  [{:keys [account-id item-refs id] :as recon}]
  (throw (UnsupportedOperationException. "reconciliations/delete is deprecated"))
  #_(with-transacted-storage (env :db)
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
