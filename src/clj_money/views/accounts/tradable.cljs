(ns clj-money.views.accounts.tradable
  "Shows a tradable account (the shares of one commodity held in an
  investment account) in tabs, its lots and its transactions, in place of
  the register."
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.accounts :refer [system-tagged?]]
            [clj-money.components :refer [load-on-scroll
                                          fill-remaining-height]]
            [clj-money.views.accounts.items :as items]
            [clj-money.views.accounts.lots :refer [lots-table]]
            [clj-money.views.accounts.fund-transactions :refer [fund-transactions-table]]))

(defn tradable-account-items
  "Shows the account in the page state's :view-account, when it's tradable
  and nothing else is open over it.

  callbacks is a map of :on-new-trade, :on-reconcile, :on-reconciliations
  and :on-back, for the buttons beneath the tabs"
  [page-state {:keys [on-new-trade on-reconcile on-reconciliations on-back]}]
  (let [account (r/cursor page-state [:view-account])
        trade (r/cursor page-state [:trade])
        reconciliation (r/cursor page-state [:reconciliation])
        reconciliation-list (r/cursor page-state [:reconciliation-list])
        show? (make-reaction #(and (system-tagged? @account :tradable)
                                   (not @trade)
                                   (not @reconciliation)
                                   (not @reconciliation-list)))
        current-nav (r/atom :lots)
        all-items-fetched? (r/cursor page-state [:all-items-fetched?])]
    (fn []
      (when @show?
        [:section
         (bs/nav-tabs [{:id :lots-nav
                        :label "Lots"
                        :elem-key :lots
                        :active? (= :lots @current-nav)
                        :nav-fn #(do
                                   (items/stop-item-loading page-state)
                                   (reset! current-nav :lots))}
                       {:id :transactions-nav
                        :label "Transactions"
                        :elem-key :transactions
                        :active? (= :transactions @current-nav)
                        :nav-fn #(reset! current-nav :transactions)}])
         [fill-remaining-height {:class "d-flex flex-column"
                                 :vertical-padding 32}
          [:div#tradable-items-container.flex-grow-1.overflow-auto
           (case @current-nav
             :lots         [lots-table @account]
             :transactions [fund-transactions-table page-state])]
          [:div.row.mt-2 {:style {:flex :none}}
           [:div.col
            [:button.btn.btn-primary
             {:title "Click here to buy or sell this commodity."
              :on-click on-new-trade}
             (icon-with-text :plus "Buy/Sell")]
            [:button.btn.btn-secondary.ms-2
             {:title "Click here to reconcile this account."
              :on-click on-reconcile}
             (icon-with-text :list-check "Reconcile")]
            [:button.btn.btn-secondary.ms-2
             {:title "Click here to see this account's reconciliations and their statements."
              :on-click on-reconciliations}
             (icon-with-text :journal-check "Reconciliations")]
            [:button.btn.btn-secondary.ms-2
             {:title "Click here to return the the account list."
              :on-click on-back}
             (icon-with-text :arrow-left-short "Back")]]
           (when (= :transactions @current-nav)
             [:div.col-auto.text-end
              [load-on-scroll {:target "tradable-items-container"
                               :all-items-fetched? @all-items-fetched?
                               :load-fn #(items/fetch-more page-state)}]])]]]))))
