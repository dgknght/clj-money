(ns clj-money.views.transactions
  (:require [clojure.set :refer [rename-keys]]
            [cljs.core.async :refer [>! go]]
            [reagent.core :as r]
            [reagent.format :refer [currency-format]]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [clj-money.bootstrap :as bs]
            [clj-money.util :as util]
            [clj-money.x-platform.util :refer [serialize-date]]
            [clj-money.x-platform.accounts :refer [polarize-quantity]]
            [clj-money.x-platform.transactions :refer [simplify
                                                       can-simplify?
                                                       entryfy]]
            [clj-money.plain-forms :as forms]
            [clj-money.components :refer [load-in-chunks]]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.notifications :as notify]))

(defn- prepare-transaction-for-edit
  [transaction account]
  (if (can-simplify? transaction)
      [(simplify transaction account) :simple]
      [(entryfy transaction) :full]))

(defn- item->tkey
  [item]
  (-> item
      (select-keys [:transaction-id :transaction-date])
      (rename-keys {:transaction-id :id})))

(defn- edit-transaction
  [item page-state]
  (transactions/get-one (item->tkey item)
                        (fn [result]
                          (let [[prepared mode] (prepare-transaction-for-edit
                                                  result
                                                  (:view-account @page-state))]
                            (swap! page-state
                                   assoc
                                   :transaction prepared
                                   :transaction-entry-mode mode))
                          (util/set-focus "transaction-date"))
                        notify/danger))

(defn stop-item-loading
  [page-state]
  (when-not (:all-items-fetched? @page-state)
    (go (>! (:ctl-chan @page-state) :quit))))

(defn load-unreconciled-items
  [page-state]
  (let [account (:view-account @page-state)]
    (transaction-items/search
      {:account-id (:id account)
       :unreconciled true
       :include-children (:include-children? @page-state)}
      #(swap! page-state assoc :items %)
      (notify/danger-fn "Unable to load the unreconciled items: %s"))))

(defn init-item-loading
  [page-state]
  (let [account (:view-account @page-state)
        end (t/last-day-of-the-month (or (:latest-transaction-date account)
                                          (t/today))) ; This is probably only nil for newly imported entities
        start (t/first-day-of-the-month (or (:earliest-transaction-date account)
                                                 (t/minus- end (t/months 6))))]
    (swap! page-state dissoc :items :all-items-fetched?)
    (load-in-chunks
      {:start start
       :end end
       :ctl-chan (:ctl-chan @page-state)
       :fetch-fn (fn [date-range callback-fn]
                   (transaction-items/search
                     {:account-id (:id account)
                      :transaction-date date-range}
                     callback-fn
                     (notify/danger-fn "Unable to fetch transaction items: %s")))
       :receive-fn #(swap! page-state update-in [:items] (fnil concat []) %)
       :finish-fn #(swap! page-state assoc :all-items-fetched? true)})))

(defn reset-item-loading
  [page-state]
  (swap! page-state dissoc :items :transaction)
  (stop-item-loading page-state)
  (init-item-loading page-state))

(defn- delete-transaction
  [item page-state]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (transactions/delete (item->tkey item)
                         #(reset-item-loading page-state)
                         notify/danger)))

(defn- item-row
  [item page-state]
  (let [account (r/cursor page-state [:view-account])
        reconciliation (r/cursor page-state [:reconciliation])]
    ^{:key (str "item-row-" (:id item))}
    [:tr.d-flex
     [:td.col-2.text-right (util/format-date (:transaction-date item))]
     [:td.col-3 (:description item)]
     [:td.col-2.text-right (currency-format (polarize-quantity item @account))]
     [:td.col-1.text-center
      (if @reconciliation
        [forms/checkbox-input reconciliation [:item-refs (:id item)]]
        (bs/icon
          (case (:reconciliation-status item)
            :completed :check-box
            :new       :dot
            :unchecked-box)))]
     (when-not @reconciliation
       [:td.col-2.text-right (currency-format (:balance item))])
     [:td.col-2
      [:div.btn-group
       [:button.btn.btn-info.btn-sm {:on-click #(edit-transaction item page-state)
                                   :title "Click here to edit this transaction."}
        (bs/icon :pencil)]
       [:button.btn.btn-danger.btn-sm {:on-click #(delete-transaction item page-state)
                                       :title "Click here to remove this transaction."}
        (bs/icon :x-circle)]]]]))

(defn items-table
  [page-state]
  (let [items (r/cursor page-state [:items])
        include-children? (r/cursor page-state [:include-children?])
        account (r/cursor page-state [:view-account])
        reconciliation (r/cursor page-state [:reconciliation])
        filter-fn (make-reaction (fn []
                                   (if @include-children?
                                     identity
                                     #(= (:id @account)
                                         (:account-id %)))))]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr.d-flex
         [:th.col-2.text-right "Date"]
         [:th.col-3 "Description"]
         [:th.col-2.text-right "Amount"]
         [:th.col-1.text-center "Rec."]
         (when-not @reconciliation
           [:th.col-2.text-right "Balance"])
         [:th.col-2 (util/space)]]]
       [:tbody
        (if @items
          (->> @items
               (filter @filter-fn)
               (map #(item-row % page-state))
               doall)
          [:tr [:td {:col-span 4} [:span.inline-status "Loading..."]]])]])))

(defn fund-transactions-table
  [page-state]
  (let [items (r/cursor page-state [:items])
        account  (r/cursor page-state [:view-account])]
    ; I don't think we need to chunk this, but maybe we do
    (transaction-items/search {:account-id (:id @account)
                               :transaction-date [:between
                                                  (:earliest-transaction-date @account)
                                                  (:latest-transaction-date @account)]}
                              #(swap! page-state assoc :items %)
                              (notify/danger-fn "Unable to load the transaction items: %s"))
    (fn []
      [:table.table.table-hover.table-borderless
       [:thead
        [:tr
         [:th.text-right "Transaction Date"]
         [:th "Description"]
         [:th.text-right "Qty."]
         [:th.text-right "Bal."]
         [:th.text-right "Value"]]]
       [:tbody
        (doall (for [item (sort-by (comp serialize-date :transaction-date) @items)]
                 ^{:key (str "item-" (:id item))}
                 [:tr
                  [:td.text-right (util/format-date (:transaction-date item))]
                  [:td (:description item)]
                  [:td.text-right (util/format-decimal (polarize-quantity item @account) 4)]
                  [:td.text-right (util/format-decimal (:balance item), 4)]
                  [:td.text-right (currency-format (:value item))]]))]])))
