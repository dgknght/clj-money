(ns clj-money.views.transaction-search
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.format :refer [currency-format]]
            [dgknght.app-lib.web :refer [format-date]]
            [dgknght.app-lib.forms :as forms]
            [clj-money.state :refer [app-state
                                     accounts
                                     accounts-by-id
                                     busy?
                                     +busy
                                     -busy]]
            [clj-money.dates :as dates]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.views.transactions :refer [navigate-to-transaction]]
            [clj-money.api.transactions :as transactions]))

(def default-filters
  {:date-range "today"})

(defn ->criteria
  ([filters] (->criteria filters (dates/today)))
  ([{:keys [description date-range quantity account]} today]
   (cond-> {:transaction/transaction-date (apply vector :between (dates/resolve-date-range date-range today))}
     (seq description) (assoc :transaction/description description)
     quantity           (assoc :transaction-item/quantity quantity)
     (:id account)      (assoc :transaction-item/account account))))

(defn- search
  [page-state]
  (let [criteria (->criteria @(r/cursor page-state [:filters]))]
    (when (seq criteria)
      (swap! page-state assoc :results nil :state :searching)
      (+busy)
      (transactions/select criteria
                           :callback -busy
                           :on-success #(swap! page-state assoc
                                               :results %
                                               :state :searched)))))

(defn- result-row
  [{:transaction/keys [transaction-date description value] :as trx} filters]
  ^{:key (str "search-result-" (:id trx))}
  [:tr {:style {:cursor "pointer"}
        :title "Click here to view this transaction."
        :on-click #(navigate-to-transaction trx (get-in filters [:account :id]))}
   [:td (format-date transaction-date)]
   [:td description]
   [:td.text-end (currency-format value)]])

(defn- results-table
  [page-state]
  (let [results (r/cursor page-state [:results])
        state (r/cursor page-state [:state])
        filters (r/cursor page-state [:filters])]
    (fn []
      (case @state
        :still     nil
        :searching [:div.d-flex.flex-column.align-items-center
                    [:div.spinner-border {:role :status}]]
        :searched  (if (seq @results)
                     [:table.table.table-hover
                      [:thead
                       [:tr
                        [:th "Date"]
                        [:th "Description"]
                        [:th.text-end "Amount"]]]
                      [:tbody
                       (doall (map #(result-row % @filters) @results))]]
                     [:div.alert.alert-info "No transactions matched the specified criteria."])))))

(defn- search-form
  [page-state]
  (let [filters (r/cursor page-state [:filters])]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (search page-state))}
       [forms/text-field filters [:description] {:caption "Description"}]
       [forms/select-field filters [:date-range] dates/date-range-options {:caption "Date"}]
       [forms/decimal-field filters [:quantity] {:caption "Amount"
                                                 :fraction-digits 2}]
       [forms/typeahead-field
        filters
        [:account :id]
        {:caption "Account"
         :search-fn (fn [input callback]
                      (callback (find-by-path input @accounts)))
         :caption-fn #(string/join "/" (:account/path %))
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback (@accounts-by-id id)))}]
       [:button.btn.btn-primary {:type :submit
                                 :disabled @busy?
                                 :title "Click here to search for transactions matching the specified criteria"}
        (if @busy?
          [:div.d-flex.align-items-center
           [:div.spinner-border.spinner-border-sm {:role :status}
            [:span.visually-hidden "Searching..."]]
           [:span.ms-1 "Search"]]
          (icon-with-text :search "Search"))]])))

(defn- index []
  (let [page-state (r/atom {:filters default-filters
                            :state :still})]
    (fn []
      [:div.mt-3
       [:h1 "Transaction Search"]
       [search-form page-state]
       [:div.mt-3
        [results-table page-state]]])))

(secretary/defroute "/search" []
  (swap! app-state assoc :page #'index :active-nav :search))
