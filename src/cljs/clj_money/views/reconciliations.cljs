(ns clj-money.views.reconciliations
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.forms :as forms]
            [clj-money.accounts :as accounts]
            [clj-money.views.transactions :as trns]
            [clj-money.api.reconciliations :as recs]))

(defn- receive-reconciliation
  [page-state reconciliation]
  (swap! page-state assoc
         :reconciliation (if reconciliation
                           (update-in reconciliation
                                      [:item-refs]
                                      (fn [item-refs]
                                        (reduce #(assoc %1 (first %2) true)
                                                {}
                                                item-refs)))
                           {:account-id (get-in @page-state
                                                [:view-account :id])})))

(defn load-working-reconciliation
  [page-state]
  (recs/find {:account-id (get-in @page-state [:view-account :id])
              :status "new"}
             (partial receive-reconciliation page-state)
             (notify/danger-fn "Unable to load the working reconciliation: %s")))

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
  (recs/save (update-in (get-in @page-state [:reconciliation])
                        [:item-refs]
                        #(resolve-item-refs % (:items @page-state)))
             (fn [_created]
               (swap! page-state dissoc :reconciliation)
               (trns/reset-item-loading page-state))
             (notify/danger-fn "Unable to save the reconciliation: %s")))

(defn- save-reconciliation
  [page-state]
  (swap! page-state assoc-in [:reconciliation :status] :new)
  (save-reconciliation* page-state))

(defn- load-previous-balance
  [page-state]
  (recs/find (-> (get-in @page-state [:view-account])
                 (accounts/->criteria {:date-field :end-of-period})
                 (assoc :desc :end-of-period
                        :status :completed))
             #(swap! page-state assoc
                      :previous-reconciliation (or %
                                                   {:balance 0}))
             (notify/danger-fn "Unable to load the previous reconciliation: %s")))

(defn- finish-reconciliation
  [page-state]
  (swap! page-state assoc-in [:reconciliation :status] :completed)
  (save-reconciliation* page-state))

(defn reconciliation-form
  [page-state]
  (let [reconciliation (r/cursor page-state [:reconciliation])
        account (r/cursor page-state [:view-account])
        previous-balance (r/cursor page-state [:previous-reconciliation :balance])
        item-ids (make-reaction #(->> (:item-refs @reconciliation)
                                      (filter second)
                                      (map first)
                                      set))
        items (r/cursor page-state [:items])
        reconciled-total (make-reaction (fn []
                                          (->> @items
                                               (filter #(@item-ids (:id %)))
                                               (map :polarized-quantity)
                                               (reduce decimal/+ 0))))
        working-balance (make-reaction #(decimal/+ @previous-balance
                                                   @reconciled-total))
        difference (make-reaction #(decimal/- (:balance @reconciliation)
                                              @working-balance))
        balanced? (make-reaction #(and (decimal/zero? @difference)
                                       (seq @item-ids)))]
    (load-previous-balance page-state)
    (fn []
      [:div.card
       [:div.card-header [:strong "Reconcile"]]
       [:div.card-body
        [forms/date-field reconciliation [:end-of-period]]
        [forms/decimal-field reconciliation [:balance]]
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
         (bs/icon-with-text :check "Finish")]
        (html/space)
        [:button.btn.btn-info {:on-click #(save-reconciliation page-state)}
         (bs/icon-with-text :download "Save")]
        (html/space)
        [:button.btn.btn-secondary {:on-click (fn []
                                                (swap! page-state dissoc :reconciliation)
                                                (trns/reset-item-loading page-state))
                                    :title "Click here to cancel this reconciliation."}
         (bs/icon-with-text :x "Cancel")]]])))
