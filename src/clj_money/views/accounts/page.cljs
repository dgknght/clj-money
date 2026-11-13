(ns clj-money.views.accounts.page
  "The Accounts page, /accounts. Lists the entity's accounts and shows the
  one the user selects, in its register (or, for a tradable account, its
  lots and transactions), where transactions and trades are entered and the
  account is reconciled. The page holds the state the elements share and
  decides which of them are shown. /accounts/:id/reconcile (see
  reconcile-path) opens the page with the account's reconciliation started."
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [chan]]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.components :refer [button]]
            [clj-money.api.commodities :as commodities]
            [clj-money.cached-accounts :refer [fetch-accounts
                                               push-transaction-date!
                                               when-accounts-loaded]]
            [clj-money.accounts :refer [system-tagged?]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     +busy
                                     -busy
                                     busy?]]
            [clj-money.views.accounts.list :refer [accounts-table]]
            [clj-money.views.accounts.filter :refer [account-filter
                                                     filter-toggle]]
            [clj-money.views.accounts.form :refer [account-form]]
            [clj-money.views.accounts.bulk-edit :refer [bulk-edit-form]]
            [clj-money.views.accounts.allocation :refer [asset-allocation
                                                         new-allocation]]
            [clj-money.views.accounts.items :as items]
            [clj-money.views.accounts.register :refer [register
                                                       recent-transactions-table]]
            [clj-money.views.accounts.tradable :refer [tradable-account-items]]
            [clj-money.views.accounts.attachments :refer [attachments-card
                                                          pending-attachment-form]]
            [clj-money.views.transactions.form :as tf]
            [clj-money.views.trades.form :as trades]
            [clj-money.views.reconciliations :as recs]
            [clj-money.views.attachments :as atts]))

(defn reconcile-path
  "Returns the path of the accounts page with the reconciliation of the
  account started"
  [{:keys [id]}]
  (str "/accounts/" (js/encodeURIComponent id) "/reconcile"))

(defn- view-account
  [account page-state]
  (swap! page-state
         assoc
         :view-account
         (update-in account
                    [:account/commodity]
                    (comp (:commodities @page-state)
                          :id))))

(defn- select-account
  [account page-state]
  (view-account account page-state)
  ; a tradable account's lots and transactions load themselves when
  ; they're shown
  (when-not (system-tagged? account :tradable)
    (items/init-item-loading page-state)))

(defn- start-reconciliation
  [page-state]
  (items/stop-item-loading page-state)
  (recs/load-working-reconciliation page-state)
  (recs/load-previous-balance page-state)
  (items/load-unreconciled-items page-state)
  (set-focus "end-of-period"))

(defn- reconcile-account
  [account page-state]
  (view-account account page-state)
  (start-reconciliation page-state))

(defn- edit-account
  [account page-state]
  (swap! page-state assoc :selected account)
  (set-focus "parent-id"))

(def ^:private account-actions
  {:view select-account
   :edit edit-account
   :reconcile reconcile-account})

(defn- open-account-by-id
  "Views, edits or reconciles the account with the id, which comes from the
  path, so it's a string. The cached accounts may still be loading, e.g.,
  when the page is loaded directly."
  [id action page-state]
  (when-accounts-loaded
    (fn []
      (when-let [account (some #(when (= id (str (:id %))) %)
                               @accounts)]
        ((account-actions action) account page-state)))))

(defn- unselect-account
  [page-state]
  (items/stop-item-loading page-state)
  (swap! page-state dissoc
         :view-account
         :attachments-item
         :reconciliation
         :items
         :all-items-fetched?))

(defn- new-transaction
  [page-state]
  (swap! page-state assoc
         :transaction #:transaction{:entity @current-entity
                                    :transaction-date (t/today)
                                    :account (:view-account @page-state)})
  (set-focus "transaction-date"))

(defn- start-trade
  [page-state new-fn]
  (swap! page-state assoc :trade (new-fn (:view-account @page-state)))
  (set-focus "transaction-date"))

(defn- extract-trx-date
  [save-result]
  (or (:transaction/transaction-date save-result)
      (get-in save-result [:transfer/transaction :transaction/transaction-date])
      (->> (:trade/transactions save-result)
           (map :transaction/transaction-date)
           first)))

(defn- reload-items
  [page-state]
  (if (:reconciliation @page-state)
    (items/load-unreconciled-items page-state)
    (items/reset-item-loading page-state)))

(defn- post-transaction-save
  [page-state]
  (fn [result]
    (let [updated-account (push-transaction-date! (:view-account @page-state)
                                                   (extract-trx-date result))]
      (swap! page-state
             (fn [state]
               (-> state
                   (dissoc :transaction :trade)
                   (assoc :view-account updated-account)))))
    (reload-items page-state)))

(defn- post-transaction-delete
  [page-state]
  (fn [_]
    (swap! page-state dissoc :transaction)
    (reload-items page-state)))

(defn- transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        view-account (r/cursor page-state [:view-account])]
    (fn []
      [tf/transaction-form transaction
       :account @view-account
       :on-save (post-transaction-save page-state)
       :on-cancel #(swap! page-state dissoc :transaction)
       :on-delete (post-transaction-delete page-state)])))

(defn- transaction-form-container
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        reconciliation (r/cursor page-state [:reconciliation])]
    (fn []
      (when @transaction
        (if @reconciliation
          [:<>
           [:div.modal.show.d-block {:tab-index -1
                                     :role :dialog}
            [:div.modal-dialog.modal-lg
             [:div.modal-content
              [:div.modal-body
               [transaction-form page-state]]]]]
           [:div.modal-backdrop.show {:on-click #(swap! page-state dissoc :transaction)}]]
          [transaction-form page-state])))))

(defn- load-commodities
  [page-state & {:keys [on-loaded]}]
  (when @current-entity
    (+busy)
    (commodities/select {}
                        :on-success (fn [commodities]
                                      (swap! page-state
                                             assoc
                                             :commodities (index-by :id commodities))
                                      (when on-loaded (on-loaded)))
                        :callback -busy)))

(defn- any-non-zero-balances?
  []
  (->> @accounts
       (map :account/value)
       (not-every? #(= 0 %))))

(defn- reset-for-entity
  "Closes whatever is open when the user switches to another entity"
  [page-state]
  (swap! page-state
         #(-> %
              (dissoc :selected
                      :view-account
                      :commodities
                      :all-items-fetched?
                      :items)
              (assoc
                :expanded #{}
                ; the accounts of the new entity aren't loaded yet
                :filter {:hide-zero-balances? true
                         :show-hidden? false
                         :tags #{}})))
  (load-commodities page-state))

(defn- account-callbacks
  "Callbacks for the buttons that act on the account being viewed"
  [page-state]
  {:on-new-transaction #(new-transaction page-state)
   :on-new-trade #(start-trade page-state trades/new-trade)
   :on-new-dividend #(start-trade page-state trades/new-dividend)
   :on-reconcile #(start-reconciliation page-state)
   :on-back #(unselect-account page-state)})

(defn- index
  [& [account-id action]]
  (let [page-state (r/atom {:expanded #{}
                            :filter {:hide-zero-balances? (any-non-zero-balances?)
                                     :show-hidden? false
                                     :tags #{}}
                            :ctl-chan (chan)})
        default-commodity (make-reaction
                            #(get-in @current-entity
                                     [:entity/settings
                                      :settings/default-commodity]))
        view-account (r/cursor page-state [:view-account])
        reconciliation (r/cursor page-state [:reconciliation])
        selected (r/cursor page-state [:selected])
        commodities (r/cursor page-state [:commodities])
        transaction (r/cursor page-state [:transaction])
        trade (r/cursor page-state [:trade])
        filter-options (r/cursor page-state [:filter])
        expanded (r/cursor page-state [:expanded])
        bulk-edit (r/cursor page-state [:bulk-edit])
        allocation (r/cursor page-state [:allocation])
        hide-zero-balances? (r/cursor filter-options [:hide-zero-balances?])
        hide-table? (make-reaction #(or @selected
                                        @view-account
                                        @allocation
                                        @transaction))
        hide-funnel? (make-reaction #(or @selected @view-account))
        callbacks (account-callbacks page-state)
        close-account-form #(swap! page-state dissoc :selected :allocation)
        on-account-save (fn [_saved]
                          (fetch-accounts)
                          (close-account-form))]
    ; the account's commodity is needed to view or reconcile it
    (load-commodities page-state
                      :on-loaded (when account-id
                                   #(open-account-by-id account-id
                                                        action
                                                        page-state)))
    (add-watch current-entity
               ::index
               (fn [_ _ previous current]
                 ; the entity may be replaced by an updated copy of itself,
                 ; which shouldn't close what's open
                 (when-not (id= previous current)
                   (reset-for-entity page-state))))
    (fn [& _]
      [:div.h-100
       [:div.row
        [:div.col-lg-8
         [:div.d-flex.justify-content-between.align-items-center
          [:h1.mt-3.accounts-title.me-auto (str "Accounts" (when @view-account
                                                             (str " - " (:account/name @view-account))))]
          (when-not @hide-funnel? [filter-toggle])]]]

       [account-filter filter-options]

       [:div.row {:class (when @hide-table? "d-none")}
        [:div.col-lg-8
         [accounts-table
          :filter @filter-options
          :expanded expanded
          :bulk-edit bulk-edit
          :on-view #(select-account % page-state)
          :on-edit #(edit-account % page-state)
          :on-reconcile #(reconcile-account % page-state)
          :on-allocate #(swap! page-state assoc :allocation (new-allocation %))]
         [button {:html {:class "btn-primary"
                         :on-click (fn []
                                     (swap! page-state
                                            assoc :selected
                                            {:account/entity (util/->entity-ref @current-entity)
                                             :account/type :asset
                                             :account/commodity @default-commodity})
                                     (set-focus "parent-id"))
                         :disabled @busy?}
                  :caption "Add"
                  :icon :plus}]]
        (when (seq (:account-ids @bulk-edit))
          [:div.col-lg-4
           [bulk-edit-form bulk-edit
            :on-save #(swap! page-state dissoc :bulk-edit)
            :on-cancel #(swap! page-state dissoc :bulk-edit)]])]
       (when @selected
         [:div.row
          [:div.col-md-6
           [:h2 (if (:id @selected) "Edit" "New")]
           [account-form selected
            :commodities @commodities
            :on-save on-account-save
            :on-cancel close-account-form]]])
       [:div.row
        [:div {:class (if @reconciliation "col-lg-8" "col")}
         [register page-state callbacks]]
        (when @reconciliation
          [:div.col-lg-4
           [recs/reconciliation-form page-state]])]
       [recs/payment-form page-state]
       [tradable-account-items page-state callbacks]
       [transaction-form-container page-state]
       (when @trade
         [:div.row
          [:div.col-md-6
           [trades/trade-form trade
            :commodities @commodities
            :on-save (post-transaction-save page-state)
            :on-cancel #(swap! page-state dissoc :trade)]]
          [:div.col-md-6
           [recent-transactions-table page-state]]])
       [attachments-card page-state]
       [atts/attachment-form page-state]
       [pending-attachment-form page-state]
       (when @allocation
         [asset-allocation allocation
          :hide-zero-balances? @hide-zero-balances?
          :on-save on-account-save
          :on-cancel close-account-form])])))

(secretary/defroute "/accounts" []
  (swap! app-state assoc :page #'index :active-nav :accounts))

;; Regex routes, because secretary (1.2.3) fails to extract the params of a
;; route like "/accounts/:id/reconcile" (see the transactions page). The
;; bindings must be plain symbols: defroute also uses them as map keys.
(secretary/defroute #"/accounts/([^/?#]+)/reconcile" [id]
  (swap! app-state assoc
         :page #(vector index (js/decodeURIComponent id) :reconcile)
         :active-nav :accounts))

(secretary/defroute #"/accounts/([^/?#]+)/edit" [id]
  (swap! app-state assoc
         :page #(vector index (js/decodeURIComponent id) :edit)
         :active-nav :accounts))

(secretary/defroute #"/accounts/([^/?#]+)" [id]
  (swap! app-state assoc
         :page #(vector index (js/decodeURIComponent id) :view)
         :active-nav :accounts))
