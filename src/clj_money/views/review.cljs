(ns clj-money.views.review
  "The page listing the transactions awaiting review, read from receipts and
  statements. Each opens on the transaction page, where saving it accepts it
  and, for one read from a receipt, rejecting it deletes it."
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.util :as util]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.api.transactions :as trn]
            [clj-money.api.receipt-ingestions :as ri]
            [clj-money.pending-review :as pending-review]
            [clj-money.views.transactions.page :as trx-page]))

(defn- load-pending
  "Loads the transactions awaiting review, each marked ::receipt? if it was
  read from a receipt, and refreshes their count in the nav"
  [page-state]
  (+busy)
  (pending-review/load-count)
  (trn/select {:transaction/review-status :pending
               :include-items true}
              :callback -busy
              :on-success
              (fn [trxs]
                (if (seq trxs)
                  (ri/select-for-transactions
                    trxs
                    :on-success
                    (fn [ingestions]
                      (let [receipt-ids (->> ingestions
                                             (map (comp :id :receipt-ingestion/transaction))
                                             set)]
                        (swap! page-state
                               assoc
                               :transactions
                               (mapv #(assoc % ::receipt? (contains? receipt-ids (:id %)))
                                     trxs)))))
                  (swap! page-state assoc :transactions [])))))

(defn- review-path
  "The path of the transaction page for the transaction, showing it from the
  perspective of its asset or liability account. It's rendered with the row,
  possibly before the accounts are loaded (accounts-by-id is nil until then),
  in which case the page shows it from its first account."
  [{:transaction/keys [items] :as trx}]
  (trx-page/path
    trx
    :account-id (->> items
                     (map #(get @accounts-by-id
                                (get-in % [:transaction-item/account :id])))
                     (filter (comp #{:asset :liability} :account/type))
                     first
                     :id)))

(defn- pending-row
  [{:keys [id] ::keys [receipt?] :transaction/keys [transaction-date description value] :as trx}]
  ^{:key (str "pending-review-row-" id)}
  [:tr.align-middle
   [:td (format-date transaction-date)]
   ; navigation to a link within the app is handled by accountant
   [:td [:a {:href (review-path trx)
             :title "Click here to review this transaction."}
         description]]
   [:td.text-end (format-decimal value)]
   [:td.d-none.d-sm-table-cell (if receipt? "Receipt" "Statement")]])

(defn- pending-table
  [page-state]
  (let [transactions (r/cursor page-state [:transactions])]
    (fn []
      (cond
        (nil? @transactions)
        [:div.d-flex.justify-content-center (bs/spinner)]

        (empty? @transactions)
        [:p.text-body-secondary "There are no transactions awaiting review."]

        :else
        [:table.table.table-hover
         [:thead
          [:tr
           [:th "Date"]
           [:th "Description"]
           [:th.text-end "Amount"]
           [:th.d-none.d-sm-table-cell "Source"]]]
         [:tbody
          (->> @transactions
               (map pending-row)
               doall)]]))))

(defn- index []
  (let [page-state (r/atom {})]
    (load-pending page-state)
    (add-watch current-entity
               ::index
               (fn [_ _ previous current]
                 ; saving the entity's settings shouldn't reload the page
                 (when-not (util/id= previous current)
                   (swap! page-state dissoc :transactions)
                   (load-pending page-state))))
    (fn []
      [:div.mt-3
       [:h1 "Review"]
       [:p.text-body-secondary
        "Transactions read from receipts and statements. Open one to check
        it; saving it accepts it."]
       [pending-table page-state]])))

(secretary/defroute "/review" []
  (swap! app-state assoc :page #'index :active-nav :review))
