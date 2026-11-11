(ns clj-money.views.accounts.register
  "The register of the account shown on the Accounts page: its transaction
  items, loaded into the page state by clj-money.views.accounts.items, with
  buttons to edit, delete or view attachments of each, to mark them
  reconciled while reconciling, and to start a new transaction."
  (:require [cljs-time.core :as t]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.web :refer [format-date]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :refer [space]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [+busy
                                     -busy
                                     busy?]]
            [clj-money.util :refer [id=]]
            [clj-money.accounts :as acts :refer [format-quantity
                                                 system-tagged?]]
            [clj-money.components :refer [load-on-scroll
                                          fill-remaining-height
                                          button]]
            [clj-money.api.transaction-items :as trx-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.views.recent-transactions :as recent-trx]
            [clj-money.views.transactions.form :as tf]
            [clj-money.views.accounts.items :as items]
            [clj-money.views.accounts.attachments :as atts]))

(defn- edit-transaction
  [transaction-item page-state]
  (+busy)
  (transactions/get-by-transaction-item
    transaction-item
    :callback -busy
    :post-xf (comp (map first)
                   (map (tf/prepare-for-edit
                          (:view-account @page-state))))
    :on-success (fn [trx]
                  (swap! page-state assoc :transaction trx)
                  (set-focus "transaction-date"))))

(defn- delete-transaction
  [transaction-item page-state]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (+busy)
    (trx-items/delete
      transaction-item
      :callback -busy
      :on-success (fn []
                    (if (:reconciliation @page-state)
                      (do
                        (swap! page-state
                               update-in
                               [:reconciliation :clj-money.views.reconciliations/item-selection]
                               dissoc
                               (:id transaction-item))
                        (items/load-unreconciled-items page-state))
                      (items/reset-item-loading page-state))))))

(defn- item-row-buttons
  [{:as item
    :transaction/keys [attachment-count]
    :reconciliation/keys [status]}
   page-state]
  [:div.btn-group
   [:button.btn.btn-secondary.btn-sm
    {:on-click #(edit-transaction item page-state)
     :title "Click here to edit this transaction."}
    (icon :pencil :size :small)]
   [:button.btn.btn-secondary.btn-sm.d-none.d-md-block
    {:on-click #(atts/show-attachments page-state item)
     :title "Click here to view attachments for this transaction"}
    (if ((some-fn nil? zero?) attachment-count)
      (icon :paperclip :size :small)
      [:span.badge.bg-info.text-dark attachment-count])]
   [:button.btn.btn-danger.btn-sm
    (if (= :completed status)
      {:disabled true
       :title "This transaction is part of a completed reconciliation, so it can't be removed."}
      {:on-click #(delete-transaction item page-state)
       :title "Click here to remove this transaction."})
    (icon :x-circle :size :small)]])

(defn- item-row
  [{:keys [account
           reconciliation
           styles]}
   page-state]
  (fn [{:transaction-item/keys [polarized-quantity
                                balance]
        :reconciliation/keys [status]
        :transaction/keys [description
                           transaction-date
                           review-status]
        :as item}]
    (let [checked? (get-in @reconciliation [:clj-money.views.reconciliations/item-selection (:id item)])
          muted-class (when checked? "text-body-secondary")]
      ^{:key (str "item-row-" (:id item))}
      [:tr.align-middle
       (atts/drop-handlers page-state item)
       [:td.text-end
        {:class muted-class}
        [:span.d-md-none (format-date transaction-date "M/d")]
        [:span.d-none.d-md-inline (format-date transaction-date)]]
       [:td {:class muted-class
             :style (get-in styles [(:id item)])}
        [:div.d-flex.justify-content-between
         [:span description]
         (when (= :pending review-status)
           [:span.badge.text-bg-warning.ms-2
            {:title "This transaction was read from a statement or receipt. Edit it to review it, or complete the reconciliation to accept it."}
            "Review"])]]
       [:td.text-end {:class muted-class} (format-quantity polarized-quantity account)]
       [:td.text-center.d-none.d-md-table-cell
        {:class muted-class}
        (if @reconciliation
          [forms/checkbox-input
           reconciliation
           [:clj-money.views.reconciliations/item-selection (:id item)]
           {::forms/decoration ::forms/none}]
          (items/reconciliation-icon status))]
       (when-not @reconciliation
         [:td.text-end.d-none.d-md-table-cell (format-quantity balance
                                                               account)])
       [:td.d-flex.justify-content-end
        {:class muted-class}
        (if @reconciliation
          [:div.btn-group
           [:button.btn.btn-secondary.btn-sm
            {:on-click #(edit-transaction item page-state)
             :title "Click here to edit this transaction."}
            (icon :pencil :size :small)]
           [:button.btn.btn-danger.btn-sm
            {:on-click #(delete-transaction item page-state)
             :title "Click here to remove this transaction."}
            (icon :x-circle :size :small)]]
          [item-row-buttons item page-state])]])))

(defn- date-compare
  [d1 d2]
  (t/before? (or d1 (t/epoch))
             (or d2 (t/epoch))))

(defn- description-compare
  [d1 d2]
  (compare (or d1 "") (or d2 "")))

(defn- amount-compare
  [a1 a2]
  (decimal/< (or a1 (decimal/zero)) (or a2 (decimal/zero))))

(def ^:private sort-fns
  {:transaction/transaction-date date-compare
   :transaction/description description-compare
   :transaction-item/polarized-quantity amount-compare})

(defn- apply-sort-direction
  [cmp dir]
  (if (= dir :desc)
    #(cmp %2 %1)
    cmp))

(defn- sort-link
  [attr label data-type [sort-on sort-dir] page-state]
  [:span.d-flex
   [:a.me-2.text-muted.text-decoration-none
    {:href "#"
     :on-click (fn [_]
                 (if (= attr sort-on)
                   (swap! page-state
                          update-in
                          [:items-sort 1]
                          #(if (= % :desc) :asc :desc))
                   (swap! page-state assoc
                          :items-sort [attr :asc])))}
    label]
   [:span.text-muted
    {:class (when-not (= attr sort-on) "invisible")}
    (icon (keyword (str "sort-"
                        (name data-type)
                        (if (= :asc sort-dir)
                          "-up"
                          "-down-alt"))))]])

(defn- items-table
  [page-state]
  (let [raw-items (r/cursor page-state [:items])
        open-recons (r/cursor page-state [:open-reconciliation-ids])
        items (make-reaction #(items/with-reconciliation-status @raw-items @open-recons))
        styles (r/cursor page-state [:item-row-styles])
        include-children? (r/cursor page-state [:include-children?])
        account (r/cursor page-state [:view-account])
        recon (r/cursor page-state [:reconciliation])
        items-sort (r/cursor page-state [:items-sort])
        sort-on (r/cursor items-sort [0])
        sort-dir (r/cursor items-sort [1])
        sort-fn (make-reaction (fn []
                                 (when @sort-on
                                   (apply-sort-direction (sort-fns @sort-on) @sort-dir))))
        filter-fn (make-reaction (fn []
                                   (if @include-children?
                                     identity
                                     #(id= @account
                                           (:transaction-item/account %)))))]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr
         [:th.text-end (sort-link :transaction/transaction-date "Date" :numeric @items-sort page-state)]
         [:th (sort-link :transaction/description "Description" :alpha @items-sort page-state)]
         [:th.text-end (sort-link :transaction-item/polarized-quantity "Amount" :numeric @items-sort page-state)]
         [:th.text-center.d-none.d-md-table-cell "Rec."]
         (when-not @recon
           [:th.text-end.d-none.d-md-table-cell "Balance"])
         [:th (space)]]]
       [:tbody
        (cond
          (seq @items)
          (->> @items
               (filter @filter-fn)
               (#(if @sort-on (sort-by @sort-on @sort-fn %) %))
               (map (item-row {:account @account
                               :reconciliation recon
                               :styles styles}
                              page-state))
               doall)

          @items
          [:tr [:td.text-center {:col-span 6} "No transactions"]]

          :else
          [:tr [:td.text-center {:col-span 6} (bs/spinner {:size :small})]])]])))

(defn- check-all-items
  [page-state checked?]
  (swap! page-state
         update-in
         [:reconciliation :clj-money.views.reconciliations/item-selection]
         merge
         (->> (:items @page-state)
              (map (comp #(vector % checked?)
                         :id))
              (into {}))))

(defn- create-trx-button
  [page-state {:keys [on-new-transaction on-new-trade on-new-dividend]}]
  (let [view-account (r/cursor page-state [:view-account])
        parent-only? (make-reaction #(acts/parent-only? @view-account))
        transaction (r/cursor page-state [:transaction])
        disable-trx? (make-reaction #(or @parent-only?
                                         (boolean @transaction)))]
    (fn []
      (cond
        (system-tagged? @view-account :tradable)
        [:button.btn.btn-primary
         {:title "Click here to enter a purchase or a sale"
          :type :button
          :on-click on-new-trade
          :disabled @disable-trx?
          :data-bs-toggle :dropdown
          :aria-expanded :false}
         (icon-with-text :plus "Buy/Sell")]

        (system-tagged? @view-account :trading)
        [:div {:class "btn-group"}
         [:button.btn.btn-primary
          {:title "Click here to enter a transaction"
           :type :button
           :on-click (when-not @parent-only? on-new-transaction)
           :disabled @disable-trx?
           :data-bs-toggle :dropdown
           :aria-expanded :false}
          (icon-with-text :plus "Add")]
         [:button.btn.btn-primary.dropdown-toggle.dropdown-toggle-split
          {:type :button
           :disabled @disable-trx?
           :data-bs-toggle :dropdown
           :aria-expanded :false}
          [:span.visually-hidden "Toggle Dropdown"]]
         [:ul.dropdown-menu
          [:li
           [:a.dropdown-item
            {:href "#"
             :on-click on-new-transaction}
            "Deposit/Withdrawal"]]
          [:li
           [:a.dropdown-item
            {:href "#"
             :on-click on-new-trade}
            "Buy/Sell"]]
          [:li
           [:a.dropdown-item
            {:href "#"
             :on-click on-new-dividend}
            "Dividend"]]]]

        :else
        [:button.btn.btn-primary
         {:title "Click here to enter a transaction."
          :type :button
          :on-click (when-not @parent-only? on-new-transaction)
          :disabled @disable-trx?
          :data-bs-toggle :dropdown
          :aria-expanded :false}
         (icon-with-text :plus "Add")]))))

(defn- register-buttons
  [page-state {:keys [on-reconcile on-reconciliations on-back] :as callbacks}]
  [:div.d-flex.justify-content-between
   [create-trx-button page-state callbacks]
   [button {:html {:class "btn-primary btn-secondary ms-2 d-none d-md-block"
                   :on-click on-reconcile
                   :disabled @busy?}
            :caption "Reconcile"
            :icon :list-check}]
   [button {:html {:class "btn-secondary ms-2 d-none d-md-block"
                   :on-click on-reconciliations
                   :title "Click here to see this account's reconciliations and their statements."
                   :disabled @busy?}
            :caption "Reconciliations"
            :icon :journal-check}]
   [button {:html {:class "btn-secondary ms-2"
                   :on-click on-back
                   :title "Click here to return to the account list."}
            :caption "Back"
            :icon :arrow-left-short}]])

(defn register
  "Shows the items of the account in the page state's :view-account, unless
  something else is open over them.

  callbacks is a map of :on-new-transaction, :on-new-trade,
  :on-new-dividend, :on-reconcile, :on-reconciliations and :on-back, for the
  buttons beneath the items"
  [page-state callbacks]
  (let [account (r/cursor page-state [:view-account])
        transaction (r/cursor page-state [:transaction])
        trade (r/cursor page-state [:trade])
        attachments-item (r/cursor page-state [:attachments-item])
        reconciliation (r/cursor page-state [:reconciliation])
        reconciliation-list (r/cursor page-state [:reconciliation-list])
        show? (make-reaction #(and @account
                                   (not @attachments-item)
                                   (not @reconciliation-list)
                                   (or @reconciliation
                                       (not (system-tagged? @account :tradable)))
                                   (or @reconciliation (not @transaction))
                                   (not @trade)))
        all-items-fetched? (r/cursor page-state [:all-items-fetched?])
        hide (make-reaction #(or @trade
                                 (not @reconciliation)))]
    (fn []
      (when @show?
        [:<>
         [:div.d-flex.flex-row-reverse
          {:class (when @hide "d-none")}
          [:button.btn.btn-secondary.ms-2
           {:on-click #(check-all-items page-state false)
            :title "Click here to mark all items as unreconciled"}
           (icon :square :size :small)]
          [:button.btn.btn-secondary.ms-2
           {:on-click #(check-all-items page-state true)
            :title "Click here to mark all items as reconciled"}
           (icon :check-square :size :small)]]
         [fill-remaining-height {:class "d-flex flex-column"
                                 :vertical-padding 32}
          [:div#items-container.flex-grow-1.overflow-auto
           [items-table page-state]]
          [:div.d-flex.mt-2 {:style {:flex :none}}
           [register-buttons page-state callbacks]
           [:span.ms-auto
            [load-on-scroll {:target "items-container"
                             :all-items-fetched? @all-items-fetched?
                             :load-fn #(items/fetch-more page-state)}]]]]]))))

(defn- recent-transaction-row
  [account page-state]
  (fn [{:transaction/keys [transaction-date description]
        :transaction-item/keys [polarized-quantity]
        :as item}]
    ^{:key (str "recent-trx-" (:id item))}
    [:tr
     [:td.text-end (format-date transaction-date)]
     [:td description]
     [:td.text-end (format-quantity polarized-quantity account)]
     [:td.text-end
      [:button.btn.btn-sm.btn-secondary
       {:on-click #(edit-transaction item page-state)
        :title "Click here to edit this transaction."}
       (icon :pencil :size :small)]]]))

(def ^:private recent-options-id "trade-recent-transactions-options")

(defn recent-transactions-table
  "The most recent of the loaded items, shown beside the trade form"
  [page-state]
  (let [account (r/cursor page-state [:view-account])]
    (fn []
      [recent-trx/table
       {:id recent-options-id
        :page-state page-state
        :items-path [:items]
        :row-fn (recent-transaction-row @account page-state)}])))
