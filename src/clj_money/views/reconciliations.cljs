(ns clj-money.views.reconciliations
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.components :refer [button]]
            [clj-money.state :refer [+busy
                                     -busy
                                     current-entity
                                     accounts
                                     accounts-by-id]]
            [clj-money.util :refer [id=]]
            [clj-money.accounts :as accounts-logic :refer [find-by-path]]
            [clj-money.cached-accounts :refer [fetch-accounts]]
            [clj-money.reconciliations :as reconciliations]
            [clj-money.transactions :refer [unaccountify ->unilateral]]
            [clj-money.views.transactions :as trns]
            [clj-money.api.accounts :as accounts-api]
            [clj-money.api.transactions :as transactions-api]
            [clj-money.api.reconciliations :as recs]))

(defn- receive-reconciliation
  [page-state]
  (fn [{:as recon :reconciliation/keys [items
                                        account
                                        end-of-period]}]
    (let [item-selection (->> items
                              (map (juxt :id (constantly true)))
                              (into {}))]
      (swap! page-state
             assoc
             :reconciliation
             (assoc recon
                    ::item-selection item-selection
                    :reconciliation/account (or account
                                                (:view-account @page-state))
                    :reconciliation/end-of-period (or end-of-period
                                                      (t/today)))
             :items-sort [:transaction/transaction-date :asc]))))

(defn load-working-reconciliation
  [page-state]
  (+busy)
  (recs/select {:reconciliation/status :new
                :reconciliation/account (:view-account @page-state)
                :limit 1
                :desc :reconciliation/end-of-period}
               :callback -busy
               :on-success (comp (receive-reconciliation page-state)
                                 first)))

(defn- apply-selections
  [{::keys [item-selection] :as recon} items]
  (let [item-map (index-by :id items)]
    (-> recon
        (dissoc ::item-selection)
        (assoc :reconciliation/items
               (->> item-selection
                    (filter second)
                    (map (comp item-map
                               first)))))))

(defn- save-reconciliation*
  [page-state & {:keys [on-success]}]
  (+busy)
  (let [{:keys [reconciliation items include-children?]} @page-state]
    (-> reconciliation
        (assoc :reconciliation/include-children? include-children?)
        (apply-selections items)
        (recs/save :callback -busy
                   :on-success (fn [created]
                                 (swap! page-state dissoc :reconciliation :items-sort)
                                 (trns/reset-item-loading page-state)
                                 (when on-success (on-success created)))))))

(defn- save-reconciliation
  [page-state]
  (swap! page-state assoc-in [:reconciliation :reconciliation/status] :new)
  (save-reconciliation* page-state))

(defn load-previous-balance
  [page-state]
  (+busy)
  (recs/previous-balance (:view-account @page-state)
                         :include-children? (:include-children? @page-state)
                         :callback -busy
                         :on-success (fn [r]
                                       (swap! page-state assoc
                                              :previous-reconciliation
                                              (or r
                                                  {:reconciliation/balance 0M})))))

(defn- default-payment
  "Builds the payment transaction template used to pre-populate the payment
  modal after reconciling a liability account. The payment account is
  pre-filled when the reconciled account already has one configured;
  otherwise it's left blank for the user to choose."
  [account balance]
  (-> (reconciliations/->payment account balance)
      (assoc :transaction/entity @current-entity
             :transaction/transaction-date (t/today)
             :transaction/description (str "Payment - " (:account/name account))
             :transaction/account (some->> account
                                            :account/payment-account
                                            :id
                                            (get @accounts-by-id)))))

(defn- finish-reconciliation
  [page-state]
  (let [account (:view-account @page-state)
        balance (get-in @page-state [:reconciliation :reconciliation/balance])]
    (swap! page-state assoc-in [:reconciliation :reconciliation/status] :completed)
    (save-reconciliation* page-state
                          :on-success (fn [_saved]
                                        (when (reconciliations/requires-payment? account)
                                          (swap! page-state assoc :payment (default-payment account balance)))))))

(defn- save-payment
  "Records the payment transaction and, when the chosen payment account
  differs from the one currently configured on the reconciled account,
  remembers it there for next time."
  [page-state]
  (+busy)
  (let [{:transaction/keys [account other-account] :as payment} (:payment @page-state)]
    (when-not (id= account (:account/payment-account other-account))
      (accounts-api/save (assoc other-account :account/payment-account {:id (:id account)})
                          :on-success (fn [_saved] (fetch-accounts))))
    (-> payment
        (update :transaction/quantity #(decimal/- 0M %))
        unaccountify
        ->unilateral
        (transactions-api/save
          :callback -busy
          :on-success (fn [_saved]
                        (swap! page-state dissoc :payment)
                        (trns/reset-item-loading page-state)
                        (notify/toast "Success" "The payment was recorded successfully."))))))

(defn payment-form
  [page-state]
  (let [payment (r/cursor page-state [:payment])
        cancel #(swap! page-state dissoc :payment)]
    (fn []
      (when-let [{:transaction/keys [other-account]} @payment]
        [:<>
         [:div.modal.show.d-block {:tab-index -1
                                   :role :dialog}
          [:div.modal-dialog
           [:div.modal-content
            [:div.modal-header
             [:h5.modal-title (str "Record Payment - " (:account/name other-account))]
             [:button.btn-close {:type :button
                                 :aria-label "Close"
                                 :title "Click here to cancel this payment."
                                 :on-click cancel}]]
            [:div.modal-body
             [forms/date-field payment [:transaction/transaction-date] {:validations #{::v/required}}]
             [forms/typeahead-field
              payment
              [:transaction/account]
              {:search-fn (fn [input callback]
                            (->> @accounts
                                 (remove #(id= % other-account))
                                 (find-by-path input)
                                 callback))
               :caption "Payment Account"
               :caption-fn (comp (partial string/join "/") :account/path)
               :value-fn identity
               :find-fn (fn [current callback] (callback current))
               :validations #{::v/required}}]
             [forms/decimal-field payment [:transaction/quantity] {:caption "Payment Amount"
                                                                    :fraction-digits 2
                                                                    :validations #{::v/required}}]
             [forms/text-field payment [:transaction/description] {:validations #{::v/required}}]
             [forms/text-field payment [:transaction/other-item :account-item/memo] {:caption "Confirmation #"}]]
            [:div.modal-footer
             [button {:html {:class "btn-secondary"
                             :type :button
                             :title "Click here to cancel this payment."
                             :on-click cancel}
                      :caption "Cancel"
                      :icon :x}]
             [button {:html {:class "btn-primary"
                             :type :button
                             :title "Click here to record this payment."
                             :on-click (fn []
                                         (v/validate payment)
                                         (when (v/valid? payment)
                                           (save-payment page-state)))}
                      :caption "Save"
                      :icon :check}]]]]]
         [:div.modal-backdrop.show {:on-click cancel}]]))))

(defn reconciliation-form
  [page-state]
  (let [recon (r/cursor page-state [:reconciliation])
        account (r/cursor page-state [:view-account])
        previous-balance (r/cursor page-state [:previous-reconciliation :reconciliation/balance])
        item-selection (r/cursor recon [::item-selection])
        items (r/cursor page-state [:items])
        reconciled-total (make-reaction (fn []
                                          (->> @items
                                               (filter (comp @item-selection :id))
                                               (map :transaction-item/polarized-quantity)
                                               (reduce decimal/+ 0M))))
        working-balance (make-reaction #(decimal/+ @previous-balance
                                                   @reconciled-total))
        difference (make-reaction #(decimal/- (:reconciliation/balance @recon)
                                              @working-balance))
        balanced? (make-reaction #(and (decimal/zero? @difference)
                                       (seq @item-selection)))
        disable? (make-reaction #(not @balanced?))]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (finish-reconciliation page-state))}
       [:div.card
        [:div.card-header [:strong "Reconcile"]]
        [:div.card-body
         [forms/date-field recon [:reconciliation/end-of-period]]
         [forms/decimal-field recon [:reconciliation/balance]]
         [forms/checkbox-field
          page-state
          [:include-children?]
          {:on-change (fn []
                        (trns/load-unreconciled-items page-state)
                        (load-previous-balance page-state))}]]
        [:table.table
         [:tbody
          [:tr
           [:th {:scope :col} "Previous Balance"]
           [:td.text-end
            (when @previous-balance
              (accounts-logic/format-quantity @previous-balance @account))]]
          [:tr
           [:th {:scope :col} "Reconciled"]
           [:td.text-end
            (accounts-logic/format-quantity @reconciled-total @account)]]
          [:tr
           [:th {:scope :col} "New Balance"]
           [:td.text-end
            (accounts-logic/format-quantity @working-balance @account)]]
          [:tr {:class (when @balanced? "bg-success text-white")}
           [:th {:scope :col} "Difference"]
           [:td.text-end
            (accounts-logic/format-quantity @difference @account)]]]]
        [:div.btn-group {:role :group}
         [button {:html {:class "btn-success"
                         :title "Click here to complete this reconciliation."
                         :type :submit}
                  :disabled? disable?
                  :icon :check}]
         [button {:html {:class "btn-info"
                         :title "Click here to save this reconciliation for later."
                         :type :button
                         :on-click #(save-reconciliation page-state)}
                  :icon :download}]
         [button {:html {:class "btn-secondary"
                         :title "Click here to discard this reconciliation."
                         :type :button
                         :on-click (fn []
                                     (swap! page-state dissoc :reconciliation :items-sort)
                                     (trns/reset-item-loading page-state))}
                  :icon :x}]]]])))
