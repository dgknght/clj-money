(ns clj-money.models.reconciliations
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]
            [clj-money.accounts :as acts]))

(defn- get-meta
  [recon & ks]
  (get-in (meta recon) ks))

(defn- starting-balance
  [recon]
  (or (get-meta recon
                ::last-completed
                :reconciliation/balance)
      0M))

(defn- in-balance?
  [{:reconciliation/keys [balance] :as recon}]
  (let [calculated (->> (get-meta recon ::all-items)
                        (map :transaction-item/polarized-quantity)
                        (reduce + (starting-balance recon)))]
    (= balance calculated)))

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
  (if-let [new-items (seq (get-meta reconciliation ::new-items))]
    (let [account-ids (->> (models/select
                             (util/model-type
                               (util/->model-ref account)
                               :account)
                             {:include-children? true})
                           (map :id)
                           set)]
      (->> new-items
           (map (comp :id
                      :transaction-item/account))
           (every? #(account-ids %))))
    true))

(v/reg-spec items-belong-to-account?
            {:message "All items must belong to the account being reconciled"
             :path [:reconciliation/item-refs]})

(defn- items-not-already-reconciled?
  [{:keys [id] :as recon}]
  (->> (get-meta recon ::new-items)
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
  [reconciliation]
  (let [last-completed (get-meta reconciliation ::last-completed)]
    (or (nil? last-completed)
        (t/before? (:reconciliation/end-of-period last-completed)
                   (:reconciliation/end-of-period reconciliation)))))

(v/reg-spec after-last-reconciliation?
            {:message "%s must be after that latest reconciliation"
             :path [:reconciliation/end-of-period]})

(s/def :reconciliation/account ::models/model-ref)
(s/def :reconciliation/end-of-period t/local-date?)
(s/def :reconciliation/balance decimal?)
(s/def :reconciliation/status #{:new :completed})
; TODO: Just call this :reconciliation/items?
(s/def :reconciliation/item-ref (s/tuple ::models/id t/local-date?))
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
      (models/select (util/model-type
                       {:id [:in ids]
                        :transaction/transaction-date [:between start end]}
                       :transaction-item)))
    []))

(defn- fetch-items
  [{:keys [id] :reconciliation/keys [account] :as recon}]
  (if id
    (let [accounts (models/select (util/model-type
                                    (util/->model-ref account)
                                    :account)
                                  {:include-children? true})
          criteria (assoc (acts/->>criteria accounts)
                          :transaction-item/reconciliation recon)]
      (models/select criteria))
    []))

(defn- polarize-item
  [{:as item :transaction-item/keys [quantity action account]}]
  (assoc item
         :transaction-item/polarized-quantity
         (acts/polarize-quantity quantity action account)))

; TODO: This can be improved, but this should fix the problem for now.
(defn- find-account []
  (let [cache (atom {})]
    (fn [{:keys [id]}]
      (if-let [account (@cache id)]
        account
        (let [account (models/find id :account)]
          (swap! cache assoc id account)
          account)))))

(defn- resolve-account []
  (let [find (find-account)]
    (fn [item]
      (update-in item [:transaction-item/account]
                 #(if (util/model-ref? %)
                    (find %)
                    %)))))

(defn- prepare-item []
  (comp polarize-item
        (resolve-account)))

(defn- find-last-completed
  "Returns the last completed reconciliation for an account"
  [{:reconciliation/keys [account] :as recon}]
  (when account
    (models/find-by (cond-> {:reconciliation/account account
                             :reconciliation/status :completed}
                      (:id recon) (assoc :id [:!= (:id recon)]))
                    {:sort [[:reconciliation/end-of-period :desc]]})))

(def ^:private ->item-ref*
  (juxt :id :transaction/transaction-date))

(defn- ->item-ref
  [item]
  (if (vector? item)
    item
    (->item-ref* item)))

(s/def ::item-ref (s/or :tuple :reconciliation/item-ref
                        :model (s/keys :req-un [::models/id]
                                       :req [:transaction/transaction-date])))

(s/def ::item-refs (s/coll-of ::item-ref))

(defmethod models/before-validation :reconciliation
  [{:reconciliation/keys [item-refs] :as reconciliation}]
  {:pre [(s/valid? (s/nilable ::item-refs) item-refs)]}
  (let [prep (prepare-item)
        existing-items (map prep
                            (fetch-items reconciliation))
        ignore? (->> existing-items
                     (map :id)
                     set)
        new-items (->> item-refs
                       (map ->item-ref)
                       (remove #(ignore? (first %)))
                       resolve-item-refs
                       (mapv prep))
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
  (->> (models/select (util/model-type
                        {:transaction-item/reconciliation recon}
                        :transaction))
       (mapcat (fn [{:transaction/keys [items transaction-date]}]
                 (mapv #(assoc % :transaction/transaction-date transaction-date)
                       items)))
       (filter #(util/model= account (:transaction-item/account %)))
       (map (juxt :id :transaction/transaction-date))))

(defn- has-item-refs?
  [{:reconciliation/keys [item-refs]}]
  (and (seq item-refs)
       (not-any? map? item-refs)))

(defn- append-transaction-item-refs
  [recon]
  ; we don't want to re-lookup item refs if the db implementation already
  ; keeps them with the reconciliation.
  ; we also don't want to allow any transaction-item maps in the list. They
  ; should be replaced with item-refs.
  (if (has-item-refs? recon)
    recon
    (assoc recon
           :reconciliation/item-refs
           (fetch-transaction-item-refs recon))))

(defmethod models/after-read :reconciliation
  [recon _opts]
  (when recon
    (append-transaction-item-refs recon)))

(defmethod models/before-delete :reconciliation
  [{:as recon :reconciliation/keys [account end-of-period]}]
  (when (< 0 (models/count {:reconciliation/account account
                            :reconciliation/end-of-period [:> end-of-period]}))
    (throw (ex-info "Only the most recent reconciliation may be deleted" {:reconciliation recon})))
  recon)

(defmethod prop/propagate :reconciliation
  [[{:as recon :reconciliation/keys [account]} after]]
  (when-not after
    (map #(assoc % :transaction-item/reconciliation nil)
         (models/select (assoc (acts/->criteria (models/find account :account))
                               :transaction-item/reconciliation recon)))))
