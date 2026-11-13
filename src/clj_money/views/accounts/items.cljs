(ns clj-money.views.accounts.items
  "Loads the transaction items of the account shown on the Accounts page into
  the page state: :items, :all-items-fetched?, the :ctl-chan that fetches the
  next chunk, and the :open-reconciliation-ids used to show each item's
  reconciliation status. Shared by the register, the fund transactions table
  and the reconciliation form."
  (:require [cljs.core.async :as a :refer [<! >! go go-loop]]
            [cljs-time.core :as t]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [accounts
                                     +busy
                                     -busy]]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.accounts :refer [polarize-quantity]]
            [clj-money.components :refer [load-in-chunks
                                          append-chunk]]
            [clj-money.api.transaction-items :as trx-items]
            [clj-money.api.reconciliations :as recs-api]))

(defn- polarize-quantities
  [account]
  (fn [items]
    (map (comp #(assoc %
                       :transaction-item/polarized-quantity
                       (polarize-quantity %))
               #(assoc % :transaction-item/account account))
         items)))

(defn- descendant-accounts
  [{:keys [id]}]
  (filter #(contains? (set (:account/parent-ids %)) id) @accounts))

(defn- effective-transaction-date-range
  [account include-children?]
  (->> (cons account (when include-children? (descendant-accounts account)))
       (map :account/transaction-date-range)
       (filter identity)
       (reduce (fn [r [start end]]
                 (dates/push-boundary r start end))
               [])))

(defn load-unreconciled-items
  [page-state]
  (+busy)
  (let [account (:view-account @page-state)
        include-children? (:include-children? @page-state)
        criteria {:transaction-item/account account
                  :transaction/transaction-date (apply vector
                                                       :between
                                                       (effective-transaction-date-range account include-children?))
                  :unreconciled true
                  :include-children include-children?}]
    (trx-items/select
      criteria
      :callback -busy
      :post-xf (map (polarize-quantities account))
      :on-success #(swap! page-state assoc :items %))))

(defn- fetch-items
  [xf]
  (completing
    (fn [ch criteria]
      (if criteria
        (trx-items/select criteria
                          :on-success #(xf ch %))
        (xf ch [])))))

(defn stop-item-loading
  [page-state]
  (let [{:keys [all-items-fetched? item-loader]} @page-state]
    (swap! page-state dissoc :item-loader)
    (when (and item-loader (not all-items-fetched?))
      (go (>! item-loader :quit)))))

(defn- load-open-reconciliations
  "Loads the ids of the account's reconciliations in progress, whose items
  aren't reconciled yet"
  [page-state account]
  (when account
    (recs-api/select {:reconciliation/account account
                      :reconciliation/status :new
                      :omit-items true}
                     :on-success #(swap! page-state
                                         assoc
                                         :open-reconciliation-ids
                                         (set (map :id %))))))

(defn- reconciliation-status
  [{:transaction-item/keys [reconciliation] :keys [id]} open-reconciliation-ids]
  (cond
    (nil? id) :new
    (nil? reconciliation) :unreconciled
    (contains? open-reconciliation-ids (:id reconciliation)) :in-progress
    :else :completed))

(defn with-reconciliation-status
  "Adds :reconciliation/status to each of the items"
  [items open-reconciliation-ids]
  (when items
    (map #(assoc %
                 :reconciliation/status
                 (reconciliation-status % open-reconciliation-ids))
         items)))

(defn reconciliation-icon
  [status]
  (case status
    :completed (icon :check-square :size :small)
    (:new :in-progress) (icon :dash-square :size :small)
    (icon :square :size :small)))

(defn init-item-loading
  ([page-state] (init-item-loading page-state nil))
  ([page-state {:keys [on-chunk]}]
   (let [account (get-in @page-state [:view-account])
         [first-date last-date :as range] (:account/transaction-date-range account)]
     (stop-item-loading page-state)
     (load-open-reconciliations page-state account)
     (if range
       (do (swap! page-state dissoc :items :all-items-fetched?)
           (let [{:keys [ctl-ch items-ch]} (->> (dates/desc-ranges first-date last-date (t/months 6))
                                                (load-in-chunks
                                                  {:fetch-xf (comp
                                                               (map (fn [[start end :as range]]
                                                                      (when (seq range)
                                                                        {:transaction-item/account (util/->entity-ref account)
                                                                         :transaction/transaction-date [:between> start end]})))
                                                               fetch-items)
                                                   :chunk-size 100}))
                 out-ch (a/chan
                          1
                          (map (polarize-quantities account)))
                 current? #(identical? ctl-ch (:item-loader @page-state))]
             (swap! page-state assoc :ctl-chan ctl-ch :item-loader ctl-ch)
             (a/pipe items-ch out-ch)
             (go-loop [items (<! out-ch)]
                      (when (current?)
                        (if items
                          (do
                            (swap! page-state append-chunk ctl-ch items)
                            (when on-chunk (on-chunk items))
                            (recur (<! out-ch)))
                          (swap! page-state assoc :all-items-fetched? true))))
             (go (>! ctl-ch :fetch))))
       (swap! page-state assoc :items [] :items-sort nil)))))

(defn reset-item-loading
  [page-state]
  (swap! page-state dissoc :items :transaction)
  (stop-item-loading page-state)
  (go (<! (a/timeout 250)) ; give the pipes some time to flush out
      (init-item-loading page-state)))

(defn fetch-more
  "Asks the item loader for the next chunk of items"
  [page-state]
  (go (>! (:ctl-chan @page-state) :fetch)))
