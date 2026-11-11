(ns clj-money.views.accounts.fund-transactions
  "The transactions of a tradable account (the shares of one commodity held
  in an investment account), interleaved with the account's lot notes. Starts
  loading the items into the page state when it's mounted."
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [clj-money.dates :as dates]
            [clj-money.components :refer [audit-history-popover]]
            [clj-money.api.audit :as audit]
            [clj-money.api.lot-notes :as lot-notes]
            [clj-money.views.accounts.items :as items]))

(defn- load-trx-item-audit!
  [state item]
  (audit/select item :transaction-item/quantity
                :on-success
                #(swap! state assoc-in [:audit-histories (:id item)] %)))

(defn- toggle-trx-item-audit!
  [state item]
  (swap! state update :audit-expanded
         (fn [expanded]
           (let [s (or expanded #{})
                 id (:id item)]
             (if (s id) (disj s id) (conj s id))))))

(defn- fund-transaction-row
  [{:as item
    :transaction/keys [transaction-date
                       description]
    :transaction-item/keys [polarized-quantity
                            balance
                            value]
    :reconciliation/keys [status]}
   state]
  (let [id (:id item)
        audit-history (get-in @state [:audit-histories id])
        expanded? (contains? (:audit-expanded @state) id)]
    ^{:key (str "item-" id)}
    [:tr
     [:td.text-end (format-date transaction-date)]
     [:td description]
     [:td.text-end
      [:div.d-flex.align-items-center
       [audit-history-popover
        audit-history
        expanded?
        #(toggle-trx-item-audit! state item)]
       [:div.ms-auto (format-decimal polarized-quantity 4)]]]
     [:td.text-end (format-decimal balance 4)]
     [:td.text-end (currency-format (or value polarized-quantity))]
     [:td.text-center.d-none.d-md-table-cell
      (items/reconciliation-icon status)]]))

(defn- lot-note-row
  [{:lot-note/keys [transaction-date memo] :as note}]
  ^{:key (str "note-" (:id note))}
  [:tr
   [:td.text-end (format-date transaction-date)]
   [:td.text-muted.fst-italic {:col-span 5} memo]])

(defn- row-date
  [row]
  (or (some-> (:transaction/transaction-date row)
              dates/serialize-local-date)
      (some-> (:lot-note/transaction-date row)
              dates/serialize-local-date)
      ""))

(defn fund-transactions-table
  [page-state]
  (let [state (r/atom {})
        raw-items (r/cursor page-state [:items])
        lot-notes (r/cursor state [:lot-notes])
        open-recons (r/cursor page-state [:open-reconciliation-ids])
        items (make-reaction #(items/with-reconciliation-status @raw-items @open-recons))
        all-rows (make-reaction
                   #(sort-by row-date
                             (fn [a b] (compare b a))
                             (concat @items @lot-notes)))]
    (lot-notes/select #:lot{:account (:view-account @page-state)}
                      :on-success #(swap! state assoc :lot-notes %))
    (items/init-item-loading page-state
                             {:on-chunk (fn [items]
                                          (doseq [item items]
                                            (load-trx-item-audit! state item)))})
    (fn []
      [:table.table.table-hover.table-borderless
       [:thead
        [:tr
         [:th.text-end "Transaction Date"]
         [:th "Description"]
         [:th.text-end "Qty."]
         [:th.text-end "Bal."]
         [:th.text-end "Value"]
         [:th.text-center.d-none.d-md-table-cell "Rec."]]]
       [:tbody
        (cond
          (nil? @raw-items)
          [:tr
           [:td.text-center.fw-lighter {:col-span 6}
            [:div.d-flex.justify-content-center.m2
             [:div.spinner-border {:role :status}
              [:span.visually-hidden "Loading"]]]]]

          (seq @items)
          (doall
            (for [row @all-rows]
              (if (:lot-note/transaction-date row)
                (lot-note-row row)
                (fund-transaction-row row state))))

          :else
          [:tr [:td.text-center.fw-lighter
                {:col-span 6}
                "No transaction for this commodity."]])]])))
