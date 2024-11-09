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
  [{:reconciliation/keys [account] :keys [id]}]
  (when-let [existing (when account
                        (find-working account))]
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
  {:pre [(s/valid? :reconciliation/item-refs item-refs)]}

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
  [{:as recon :reconciliation/keys [account]}]
  (->> (models/select (db/model-type
                        {:transaction-item/reconciliation recon}
                        :transaction))
       (mapcat (fn [{:transaction/keys [items transaction-date]}]
                 (mapv #(assoc % :transaction-item/transaction-date transaction-date)
                       items)))
       (filter #(util/model= account (:transaction-item/account %)))
       (map (juxt :id :transaction-item/transaction-date))))

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
