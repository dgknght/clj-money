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

(defn find-working
  "Returns the uncompleted reconciliation for the specified
  account, if one exists"
  [account]
  (entities/find-by #:reconciliation{:status :new
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

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
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

(defn- find-last-completed
  "Returns the last completed reconciliation for an account"
  [{:reconciliation/keys [account] :as recon}]
  (when account
    (entities/find-by (cond-> {:reconciliation/account account
                             :reconciliation/status :completed}
                      (:id recon) (assoc :id [:!= (:id recon)]))
                    {:sort [[:reconciliation/end-of-period :desc]]})))

(defn- polarize-item
  "Assoc :transaction-item/polarized-quantity to the item"
  [{:as item :transaction-item/keys [account action quantity]}]
  (assoc item
         :transaction-item/polarized-quantity
         (acts/polarize-quantity {:account account
                                  :action action
                                  :quantity quantity})))

(defn- account+children
  "Fetch and return the account children along with the given account"
  [account]
  (entities/select (util/entity-type
                     (util/->entity-ref account)
                     :account)
                   {:include-children? true}))

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
        ignore? (comp (->> existing-items
                           (map :id)
                           set)
                      :id)
        new-items (remove ignore? items)
        all-items (->> new-items
                       (concat existing-items)
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
