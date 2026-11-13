(ns clj-money.views.reconciliations.show
  "The page for one reconciliation (link to it with path), read-only: its
  account, end of period, balance and status, its statement, if one is
  attached, and the transaction items it covers. An unfinished
  reconciliation can be continued from it."
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.state :refer [app-state
                                     accounts-by-id]]
            [clj-money.accounts :refer [polarize-quantity]]
            [clj-money.api.reconciliations :as recs]
            [clj-money.views.attachments :refer [view-image]]
            [clj-money.views.accounts.page :refer [reconcile-path]]))

(defn path
  "Returns the path of the page for the reconciliation"
  [{:keys [id]}]
  (str "/reconciliations/" (js/encodeURIComponent id)))

(defn- account-name
  [account]
  (some->> (get @accounts-by-id (:id account))
           :account/path
           (string/join "/")))

(defn- status-badge
  [status]
  (if (= :completed status)
    [:span.badge.rounded-pill.text-bg-success "Completed"]
    [:span.badge.rounded-pill.text-bg-warning "In progress"]))

(defn- amount
  "The item's quantity vis a vis its account, once the accounts are loaded
  (accounts-by-id is nil until then)"
  [{:transaction-item/keys [quantity action account]}]
  (when-let [account (get @accounts-by-id (:id account))]
    (format-decimal (polarize-quantity quantity action account)
                    {:fraction-digits 2})))

(defn- item-row
  [{:keys [id] :transaction/keys [transaction-date description] :as item}
   show-account?]
  ^{:key (str "reconciliation-item-" id)}
  [:tr
   [:td (format-date transaction-date)]
   [:td description]
   (when show-account?
     [:td.d-none.d-md-table-cell (account-name (:transaction-item/account item))])
   [:td.text-end (amount item)]])

(defn- items-table
  [{:reconciliation/keys [items account]}]
  ; a reconciliation that includes the account's children covers items in
  ; other accounts, so they're labeled
  (let [show-account? (some #(not= (:id account)
                                   (get-in % [:transaction-item/account :id]))
                            items)]
    (if (seq items)
      [:table.table.table-hover
       [:thead
        [:tr
         [:th "Date"]
         [:th "Description"]
         (when show-account?
           [:th.d-none.d-md-table-cell "Account"])
         [:th.text-end "Amount"]]]
       [:tbody
        (doall (map #(item-row % show-account?) items))]]
      [:p.text-body-secondary "This reconciliation covers no transactions."])))

(defn- summary
  [{:reconciliation/keys [account end-of-period balance status]}]
  [:dl.row
   [:dt.col-sm-3 "Account"]
   [:dd.col-sm-9 (account-name account)]
   [:dt.col-sm-3 "End of Period"]
   [:dd.col-sm-9 (format-date end-of-period)]
   [:dt.col-sm-3 "Balance"]
   [:dd.col-sm-9 (format-decimal balance {:fraction-digits 2})]
   [:dt.col-sm-3 "Status"]
   [:dd.col-sm-9 (status-badge status)]])

(defn- actions
  [{:reconciliation/keys [account status statement]}]
  [:div.d-flex.gap-2.mb-3
   ; navigation to a link within the app is handled by accountant
   [:a.btn.btn-secondary
    {:href "/reconciliations"
     :title "Click here to return to the list of reconciliations."}
    (icon-with-text :arrow-left-short "Back")]
   (when statement
     [:button.btn.btn-outline-primary
      {:type :button
       :title "Click here to view the statement in a new tab."
       :on-click #(view-image statement)}
      (icon-with-text :file-earmark-pdf "Statement")])
   (when-not (= :completed status)
     [:a.btn.btn-primary
      {:href (reconcile-path account)
       :title "Click here to continue this reconciliation."}
      (icon-with-text :list-check "Continue")])])

(defn- page
  [id]
  (let [recon (r/atom nil)]
    (recs/get {:id id} :on-success #(reset! recon %))
    (fn []
      [:div.mt-3
       [:h1 "Reconciliation"]
       (if @recon
         [:<>
          [summary @recon]
          [actions @recon]
          [items-table @recon]]
         [:div.d-flex.justify-content-center (bs/spinner)])])))

; A regex route, because secretary (1.2.3) fails to extract the params of a
; route like "/reconciliations/:id" (see the transactions page). The
; bindings must be plain symbols: defroute also uses them as map keys.
(secretary/defroute #"/reconciliations/([^/?#]+)" [id]
  (swap! app-state assoc
         :page #(vector page (js/decodeURIComponent id))
         :active-nav :reconciliations))
