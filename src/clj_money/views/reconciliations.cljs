(ns clj-money.views.reconciliations
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [cljs-time.core :as t]
            [clj-money.components :refer [button]]
            [clj-money.state :refer [+busy
                                     -busy]]
            [clj-money.accounts :as accounts]
            [clj-money.views.transactions :as trns]
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
                                                      (t/today)))))))

(defn- ->criteria
  [recon]
  (accounts/->criteria
    recon
    {:account-attribute :reconciliation/account
     :date-attribute :reconciliation/end-of-period}))

(defn load-working-reconciliation
  [page-state]
  (+busy)
  (recs/select (-> (get-in @page-state [:view-account])
                   ->criteria
                   (assoc :desc :reconciliation/end-of-period
                          :limit 1
                          :reconciliation/status :new))
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
  [page-state]
  (+busy)
  (let [{:keys [reconciliation items]} @page-state]
    (-> reconciliation
        (apply-selections items)
        (recs/save :callback -busy
                   :on-success (fn [_created]
                                 (swap! page-state dissoc :reconciliation)
                                 (trns/reset-item-loading page-state))))))

(defn- save-reconciliation
  [page-state]
  (swap! page-state assoc-in [:reconciliation :reconciliation/status] :new)
  (save-reconciliation* page-state))

(defn- load-previous-balance
  [page-state]
  (+busy)
  (recs/select (-> (get-in @page-state [:view-account])
                   ->criteria
                   (assoc :desc :reconciliation/end-of-period
                          :reconciliation/status :completed
                          :limit 1))
               :callback -busy
               :on-success (fn [[r]]
                             (swap! page-state assoc
                                   :previous-reconciliation
                                   (or r
                                       {:reconciliation/balance 0M})))))

(defn- finish-reconciliation
  [page-state]
  (swap! page-state assoc-in [:reconciliation :reconciliation/status] :completed)
  (save-reconciliation* page-state))

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
    (load-previous-balance page-state)
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
          {:on-change #(trns/load-unreconciled-items page-state)}]]
        [:table.table
         [:tbody
          [:tr
           [:th {:scope :col} "Previous Balance"]
           [:td.text-end
            (when @previous-balance
              (accounts/format-quantity @previous-balance @account))]]
          [:tr
           [:th {:scope :col} "Reconciled"]
           [:td.text-end
            (accounts/format-quantity @reconciled-total @account)]]
          [:tr
           [:th {:scope :col} "New Balance"]
           [:td.text-end
            (accounts/format-quantity @working-balance @account)]]
          [:tr {:class (when @balanced? "bg-success text-white")}
           [:th {:scope :col} "Difference"]
           [:td.text-end
            (accounts/format-quantity @difference @account)]]]]
        [:div.card-footer
         [button {:html {:class "btn-success"
                         :type :submit}
                  :disabled? disable?
                  :icon :check
                  :caption "Finish"}]
         (html/space)
         [button {:html {:class "btn-info"
                         :type :button
                         :on-click #(save-reconciliation page-state)}
                  :icon :download
                  :caption "Save"}]
         (html/space)
         [button {:html {:class "btn-secondary"
                         :type :button
                         :on-click (fn []
                                     (swap! page-state dissoc :reconciliation)
                                     (trns/reset-item-loading page-state))
                         :title "Click here to cancel this reconciliation."}
                  :icon :x
                  :caption "Cancel"}]]]])))
