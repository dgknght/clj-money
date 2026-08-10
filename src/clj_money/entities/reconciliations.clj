(ns clj-money.entities.reconciliations
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.accounts :as acts]))

(defn- get-meta
  [recon & ks]
  (get-in (meta recon) ks))

(defn- own-last-completed
  "Returns the last completed reconciliation for exactly the given account,
  ignoring any descendants. exclude-id, when given, omits that reconciliation
  from consideration (so that validating an update to a completed
  reconciliation doesn't treat itself as its own prior balance)."
  [account exclude-id]
  (when account
    (entities/find-by (cond-> {:reconciliation/account account
                             :reconciliation/status :completed}
                      exclude-id (assoc :id [:!= exclude-id]))
                    {:sort [[:reconciliation/end-of-period :desc]]})))

(defn- absorbed-by-ancestor?
  "True if descendant's items have already been swept into a reconciliation
  belonging to one of its ancestors, at or below account (e.g. a prior
  'include children' reconciliation on a parent or grandparent already
  covered this descendant's items). When that's the case, descendant's own
  reconciliation balance is already reflected in that ancestor's balance and
  must not be added again."
  [account descendant by-id exclude-id]
  (loop [current-id (-> descendant :account/parent :id)]
    (when current-id
      (let [ancestor (by-id current-id)
            ancestor-recon (own-last-completed ancestor exclude-id)
            hit? (some #(= (:id descendant) (-> % :transaction-item/account :id))
                       (:reconciliation/items ancestor-recon))]
        (cond
          hit? true
          (= current-id (:id account)) false
          :else (recur (-> ancestor :account/parent :id)))))))

(defn- compute-starting-balance
  "The starting point for a reconciliation of account is its own last
  completed reconciliation's balance (if it has one), plus each descendant's
  own last completed reconciliation balance -- except a descendant whose
  items have already been absorbed into an ancestor's reconciliation, which
  would otherwise be double-counted. A descendant with no reconciliation of
  its own contributes nothing: a starting balance must only reflect amounts
  already confirmed via a reconciliation, never an unverified ledger
  balance."
  [account family exclude-id]
  (let [by-id (index-by :id family)
        own (own-last-completed account exclude-id)]
    (+ (or (:reconciliation/balance own) 0M)
       (->> family
            (remove #(= (:id %) (:id account)))
            (keep (fn [descendant]
                    (when-let [d-own (own-last-completed descendant exclude-id)]
                      (when-not (absorbed-by-ancestor? account descendant by-id exclude-id)
                        (:reconciliation/balance d-own)))))
            (reduce + 0M)))))

(defn- starting-balance
  [{:reconciliation/keys [account] :keys [id] :as recon}]
  (compute-starting-balance account (vals (get-meta recon ::accounts)) id))

(defn- in-balance?
  [{:reconciliation/keys [balance] :as recon}]
  (let [calculated (->> (get-meta recon ::all-items)
                        (map :transaction-item/polarized-quantity)
                        (filter identity)
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

(defn- working-reconciliation-exists?
  [{:reconciliation/keys [account] :keys [id]}]
  (when account
    (< 0 (entities/count
           (cond-> #:reconciliation{:status :new
                                    :account account}
             id (assoc :id [:!= id]))))))

(def no-working-conflict?
  (complement working-reconciliation-exists?))

(v/reg-spec no-working-conflict?
            {:message "%s already has a reconciliation in progress"
             :path [:reconciliation/account]})

(defn- items-belong-to-account?
  [reconciliation]
  (if-let [new-items (seq (get-meta reconciliation ::new-items))]
    (let [accounts (get-meta reconciliation ::accounts)
          match? (comp accounts :id :transaction-item/account)]
      (every? match? new-items))
    true))

(v/reg-spec items-belong-to-account?
            {:message "All items must belong to the account being reconciled"
             :path [:reconciliation/items]})

(defn- items-not-already-reconciled?
  [{:keys [id] :as recon}]
  (->> (get-meta recon ::new-items)
       (map (comp :id :transaction-item/reconciliation))
       (remove (some-fn nil? #(= id %)))
       empty?))

(v/reg-spec items-not-already-reconciled? {:message "No item can belong to another reconciliation"
                                           :path [:reconciliation/items]})

(defn- can-be-updated?
  [recon]
  (or (-> recon :id nil?)
      (= :new (-> recon
                  entities/find
                  :reconciliation/status))))

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

(s/def :reconciliation/account ::entities/entity-ref)
(s/def :reconciliation/end-of-period t/local-date?)
(s/def :reconciliation/balance decimal?)
(s/def :reconciliation/status #{:new :completed})

(s/def :reconciliation/item
  (s/or :abbreviated (s/keys :req-un [::entities/id])
        :full ::entities/transaction-item))

(s/def :reconciliation/items (s/coll-of :reconciliation/item))

(s/def ::entities/reconciliation
  (s/and (s/keys :req [:reconciliation/account
                       :reconciliation/end-of-period
                       :reconciliation/status
                       :reconciliation/balance]
                 :opt [:reconciliation/items])
         not-unbalanced?
         no-working-conflict?
         items-belong-to-account?
         items-not-already-reconciled?
         can-be-updated?
         after-last-reconciliation?))

(defn- fetch-items
  [{:keys [id] :reconciliation/keys [account] :as recon}]
  (if id
    (let [accounts (entities/select (util/entity-type
                                      (util/->entity-ref account)
                                      :account)
                                    {:include-children? true})
          by-id (index-by :id accounts)
          criteria (assoc (acts/->>criteria accounts)
                          :transaction-item/reconciliation recon)]
      (map #(update-in % [:transaction-item/account] (comp by-id :id))
           (entities/select criteria
                            {:datalog/hints [:transaction-item/reconciliation
                                             :transaction-item/account]})))
    []))

(defn- account+children
  "Fetch and return the account children along with the given account"
  [account]
  (entities/select (util/entity-type
                     (util/->entity-ref account)
                     :account)
                   {:include-children? true}))

(defn- find-last-completed
  "Returns the last completed reconciliation for an account or any of its
  descendants (e.g. one created against a child account during import)"
  [{:reconciliation/keys [account] :as recon}]
  (when account
    (let [ids (map :id (account+children account))]
      (entities/find-by (cond-> {:reconciliation/account [:in ids]
                                 :reconciliation/status :completed}
                          (:id recon) (assoc :id [:!= (:id recon)]))
                        {:sort [[:reconciliation/end-of-period :desc]]}))))

(defn previous-balance
  "Returns the balance to use as the starting point when reconciling the
  given account: its own last completed reconciliation's balance, plus each
  not-yet-absorbed descendant's own last completed reconciliation balance.
  See compute-starting-balance for the full rule."
  [account]
  (compute-starting-balance account (account+children account) nil))

(defn- polarize-item
  "Assoc :transaction-item/polarized-quantity to the item"
  [item]
  (assoc item
         :transaction-item/polarized-quantity
         (acts/polarize-quantity item)))

(defn- fetch-transaction-items
  ([recon]
   (entities/select {:transaction-item/reconciliation recon}
                    {:select-also :transaction/transaction-date
                     :datalog/hints [:transaction-item/reconciliation
                                     :transaction-item/transaction-item]})))

(defmethod entities/before-validation :reconciliation
  [{:reconciliation/keys [account items] :as recon}]
  {:pre [(s/valid? (s/nilable :reconciliation/items)
                   (:reconciliation/items recon))]}
  (let [accounts (when account
                   (index-by :id (account+children account)))
        existing-items (if (:id recon)
                         (fetch-transaction-items recon)
                         [])
        existing? (comp (->> existing-items
                             (map :id)
                             set)
                        :id)
        new-items (remove existing? items)
        submitted? (comp (->> items
                              (map :id)
                              set)
                         :id)
        kept-items (filter submitted? existing-items)
        removed-items (remove submitted? existing-items)
        all-items (->> new-items
                       (concat kept-items)
                       (map (comp polarize-item
                                  #(update-in %
                                              [:transaction-item/account]
                                              (comp accounts :id)))))]
    (-> recon
        (update-in [:reconciliation/status] (fnil identity :new))
        (vary-meta
          #(assoc %
                  ::accounts accounts
                  ::new-items new-items
                  ::removed-items removed-items
                  ::all-items all-items
                  ::existing-items existing-items
                  ::last-completed (find-last-completed recon))))))

(defn- append-transaction-items
  [{:as recon :reconciliation/keys [items]}]
  ; we don't want to re-lookup items if the db implementation already
  ; keeps them with the reconciliation.
  (if (seq items)
    recon
    (assoc recon
           :reconciliation/items
           (fetch-transaction-items recon))))

(defmethod entities/after-read :reconciliation
  [recon _opts]
  (when recon
    (append-transaction-items recon)))

(defmethod entities/before-delete :reconciliation
  [{:as recon :reconciliation/keys [account end-of-period]}]
  (when (< 0 (entities/count {:reconciliation/account account
                            :reconciliation/end-of-period [:> end-of-period]}))
    (throw (ex-info "Only the most recent reconciliation may be deleted" {:reconciliation recon})))
  recon)

(defmethod prop/propagate :reconciliation
  [[recon after]]
  (when-not after
    (map #(assoc % :transaction-item/reconciliation nil)
         (fetch-items recon))))
