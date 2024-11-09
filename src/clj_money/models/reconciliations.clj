(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.accounts :as acts]))

(declare find
         find-by
         find-last-completed)

(defn- get-meta
  [recon & ks]
  (get-in (meta recon) ks))

(defn- in-balance?
  [{:reconciliation/keys [balance] :as recon}]
  (let [starting-balance (or (get-meta recon
                                       ::last-completed
                                       :reconciliation/balance)
                             0M)
        new-balance (->> (::all-items (meta recon))
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

(s/def :reconciliation/account ::models/model-ref)
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
          [start end] (->> item-refs
                           (map second)
                           (sort)
                           ((juxt first last)))]
      (models/select (db/model-type
                       {:id [:in ids]
                        :transaction/transaction-date [:between start end]}
                       :transaction-item)))
    []))

(defn- fetch-items
  [{:keys [id] :reconciliation/keys [account] :as recon}]
  (if id
    (let [accounts (models/select {:transaction-item/account account}
                                  {:include-children? true})
          criteria (assoc (acts/->criteria accounts)
                          :transaction-item/reconciliation recon)]
      (models/select criteria))
    []))

(defn- find-last-completed
  "Returns the last completed reconciliation for an account"
  [{:reconciliation/keys [account] :as recon}]
  (when account
    (models/find-by (cond-> {:reconciliation/account account
                             :reconciliation/status :completed}
                      (:id recon) (assoc :id [:!= (:id recon)]))
                    {:sort [[:reconciliation/end-of-period :desc]]})))

(defn- polarize-item
  [{:as item :transaction-item/keys [quantity action account]}]
  (assoc item
         :transaction-item/polarized-quantity
         (acts/polarize-quantity quantity action account)))

(defn- resolve-account
  [item]
  (update-in item [:transaction-item/account]
             #(if (util/model-ref? %)
                (models/find % :account)
                %)))

(def ^:private prepare-item
  (comp polarize-item
        resolve-account))

(defmethod models/before-validation :reconciliation
  [{:reconciliation/keys [item-refs] :as reconciliation}]
  (let [existing-items (map prepare-item
                            (fetch-items reconciliation))
        ignore? (->> existing-items
                     (map :id)
                     set)
        new-items (->> item-refs
                       (remove #(ignore? (first %)))
                       resolve-item-refs
                       (mapv prepare-item))
        all-items (concat existing-items new-items)]
    (-> reconciliation
        (update-in [:reconciliation/status] (fnil identity :new))
        (vary-meta
          #(assoc %
                  ::new-items new-items
                  ::all-items all-items
                  ::existing-items existing-items
                  ::last-completed (find-last-completed reconciliation))))))

(defn- fetch-transaction-item-refs
  [recon]
  (->> (models/select (db/model-type
                        {:transaction-item/reconciliation recon}
                        :transaction))
       (util/pp->> ::items)
       (mapcat (fn [{:transaction/keys [items transaction-date]}]
                 (mapv (comp #(vector % transaction-date)
                             :id)
                       items)))))

(defn- append-transaction-item-refs
  [recon]
  (when recon
    (assoc recon
           :reconciliation/item-refs
           (fetch-transaction-item-refs recon))))

(defmethod models/after-read :reconciliation
  [recon _opts]
  (when recon
    (append-transaction-item-refs recon)))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (Exception. "deprecated"))))

(defn ^:deprecated find-by
  ([criteria]
   (find-by criteria {}))
  ([_criteria _options]
   (throw (Exception. "deprecated"))
   #_(first (search criteria (assoc options :limit 1)))))

(defn ^:deprecated find
  "Returns the specified reconciliation"
  [_reconciliation-or-id]
  (throw (Exception. "deprecated"))
  #_(find-by {:id (->id reconciliation-or-id)}))

(defn- ->date-range
  [items date-fn]
  (when (seq items)
    ((juxt first last) (->> items
                            (map date-fn)
                            sort))))

(defn- unreconcile-old-items
  [_ #_{:keys [id] :as reconciliation}]
  #_(when (seq (::existing-items reconciliation))
    (let [[start end] (->date-range (::existing-items reconciliation)
                                    :transaction-date)]
      (transactions/update-items {:reconciliation-id nil}
                                 {:reconciliation-id id
                                  :transaction-date [:between start end]}))))

(defn- reconcile-all-items
  [recon]
  (when-let [all-items (-> recon meta ::all-items seq)]
    (let [[start end] (->date-range all-items :transaction-item/transaction-date)]
      (models/update {:transaction-item/reconciliation (util/->model-ref recon)}
                     {:id [:in (map :id all-items)]
                      :transaction/transaction-date [:between start end]}))))

(defmethod models/after-save :reconciliation
  [recon]
  (unreconcile-old-items recon)
  (reconcile-all-items recon)
  recon)

(defn ^:deprecated create
  [_reconciliation]
  (throw (UnsupportedOperationException. "reconciliations/create is deprecated"))
  #_(with-transacted-storage (env :db)
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
  [_recon]
  (throw (UnsupportedOperationException. "reconciliations/update is deprecated"))
  #_(with-transacted-storage (env :db)
    (let [recon (before-validation recon)]
      (with-validation recon ::reconciliation
        (let [to-update (before-save recon)]
          (storage/update to-update)
          (after-save to-update)
          (reload recon))))))
