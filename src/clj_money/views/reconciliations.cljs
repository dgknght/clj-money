(ns clj-money.views.reconciliations
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [clj-money.state :refer [+busy
                                     -busy]]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.accounts :as accounts]
            [clj-money.views.transactions :as trns]
            [clj-money.api.reconciliations :as recs]))

(defn- receive-reconciliation
  [page-state]
  (fn [reconciliation]
    (swap! page-state
           assoc
           :reconciliation (if reconciliation
                             (update-in reconciliation
                                        [:reconciliation/item-refs]
                                        (fn [item-refs]
                                          (reduce #(assoc %1 (first %2) true)
                                                  {}
                                                  item-refs)))
                             {:reconciliation/account (get-in @page-state
                                                              [:view-account])}))))

(defn- ->criteria
  [recon]
  (accounts/->criteria recon
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

(defn- resolve-item-refs
  [item-refs items]
  (let [items-map (->> items
                       (map (juxt :id identity))
                       (into {}))]
    (->> item-refs
         (filter second)
         (map (comp #(vector %
                             (get-in items-map [% :transaction-date]))
                    first)))))

(defn- save-reconciliation*
  [page-state]
  (+busy)
  (recs/save (update-in (get-in @page-state [:reconciliation])
                        [:item-refs]
                        #(resolve-item-refs % (:items @page-state)))
             :callback -busy
             :on-success (fn [_created]
                           (swap! page-state dissoc :reconciliation)
                           (trns/reset-item-loading page-state))))

(defn- save-reconciliation
  [page-state]
  (swap! page-state assoc-in [:reconciliation :status] :new)
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
  (swap! page-state assoc-in [:reconciliation :status] :completed)
  (save-reconciliation* page-state))

(defn reconciliation-form
  [page-state]
  (let [reconciliation (r/cursor page-state [:reconciliation])
        account (r/cursor page-state [:view-account])
        previous-balance (r/cursor page-state [:previous-reconciliation :reconciliation/balance])
        item-ids (make-reaction #(->> (:reconciliation/item-refs @reconciliation)
                                      (filter second)
                                      (map first)
                                      set))
        items (r/cursor page-state [:items])
        reconciled-total (make-reaction (fn []
                                          (->> @items
                                               (filter (comp @item-ids :id))
                                               (map :transaction-item/polarized-quantity)
                                               (reduce decimal/+ 0M))))
        working-balance (make-reaction #(decimal/+ @previous-balance
                                                   @reconciled-total))
        difference (make-reaction #(decimal/- (:reconciliation/balance @reconciliation)
                                              @working-balance))
        balanced? (make-reaction #(and (decimal/zero? @difference)
                                       (seq @item-ids)))]
    (load-previous-balance page-state)
    (fn []
      [:div.card
       [:div.card-header [:strong "Reconcile"]]
       [:div.card-body
        [forms/date-field reconciliation [:reconciliation/end-of-period]]
        [forms/decimal-field reconciliation [:reconciliation/balance]]
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
           (accounts/format-quantity @reconciled-total @account)]]
         [:tr {:class (when @balanced? "bg-success text-white")}
          [:th {:scope :col} "Difference"]
          [:td.text-end
           (accounts/format-quantity @difference @account)]]]]
       [:div.card-footer
        [:button.btn.btn-success {:on-click #(finish-reconciliation page-state)
                                  :disabled (not @balanced?)}
         (icon-with-text :check "Finish")]
        (html/space)
        [:button.btn.btn-info {:on-click #(save-reconciliation page-state)}
         (icon-with-text :download "Save")]
        (html/space)
        [:button.btn.btn-secondary {:on-click (fn []
                                                (swap! page-state dissoc :reconciliation)
                                                (trns/reset-item-loading page-state))
                                    :title "Click here to cancel this reconciliation."}
         (icon-with-text :x "Cancel")]]])))
