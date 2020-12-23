(ns clj-money.views.budgets
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [clj-money.decimal :as decimal]
            [clj-money.state :refer [app-state]]
            [clj-money.html :as html]
            [clj-money.util :refer [format-decimal]]
            [clj-money.bootstrap :as bs]
            [clj-money.notifications :as notify]
            [clj-money.forms :as forms]
            [clj-money.budgets :as budgets]
            [clj-money.accounts :as accounts]
            [clj-money.api.accounts :as accounts-api]
            [clj-money.api.budgets :as api]))

(defn- load-budgets
  [page-state]
  (swap! page-state assoc :loading? true)
  (api/search (fn [result]
                (swap! page-state dissoc :loading?)
                (swap! page-state assoc :budgets result))
              (notify/danger-fn "Unable to load the budgets: %s")))

(defn- delete-budget
  [budget page-state]
  (when (js/confirm (str "Are you sure you want to delete the budget " (:name budget) "?"))
    (api/delete budget
                #(load-budgets page-state)
                (notify/danger-fn "Unable to remove the budget: %s"))))

(defn- load-budget-details
  [budget page-state]
  (api/find (:id budget)
            (fn [b]
              (swap! page-state assoc :detailed-budget b)
              (html/set-focus "account-id"))
            (notify/danger-fn "Unable to load the budget details: %s")))

(defn- budget-row
  [budget page-state]
  ^{:key (str "budget-row-" (:id budget))}
  [:tr
   [:td (:name budget)]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-info {:on-click (fn []
                                               (swap! page-state assoc :selected budget)
                                               (html/set-focus "name"))
                                   :title "Click here to edit this budget"}
      (bs/icon :pencil)]
     [:button.btn.btn-sm.btn-info {:on-click #(load-budget-details budget page-state)
                                   :title "Click here to fill out details for this budget."}
      (bs/icon :collection)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete-budget budget page-state)
                                     :title "Click here to remove this budget."}
      (bs/icon :x-circle)]]]])

(defn- budgets-table
  [page-state]
  (let [budgets (r/cursor page-state [:budgets])
        loading? (r/cursor page-state [:loading?])]
    (fn []
      [:table.table.table-hover
       [:thead
        [:tr
         [:th "Name"]
         [:th (html/space)]]]
       [:tbody
        (if @loading?
          [:tr
           [:td {:col-span 2} [:span.inline-status "Loading..."]]]
          (if (seq @budgets)
            (doall (map #(budget-row % page-state) @budgets))
            [:tr
             [:td {:col-span 2} [:span.inline-status "No budgets."]]]))]])))

(defn- budgets-list
  [page-state]
  [:div
   [budgets-table page-state]
   [:div.mt-2
    [:button.btn.btn-primary {:on-click (fn []
                                          (swap! page-state
                                                 assoc
                                                 :selected
                                                 {:period :month
                                                  :period-count 12})
                                          (html/set-focus "name"))}
     (bs/icon-with-text :plus "Add")]]])

(defn- save-budget
  [page-state]
  (api/save (dissoc (:selected @page-state) :items)
            (fn []
              (load-budgets page-state)
              (swap! page-state dissoc :selected))
            (notify/danger-fn "Unable to save the budget: %s")))

(defn- budget-form
  [page-state]
  (let [selected (r/cursor page-state [:selected])]
    (fn []
      [:div.card
       [:div.card-header
        [:strong (str (if (:id @selected) "Edit" "New") " Budget")]]
       [:div.card-body
        [:form {:on-submit #(.preventDefault %)
                :no-validate true}
         [forms/text-field selected [:name] {:validate [:required]}]
         [forms/date-field selected [:start-date] {:validate [:required]}]
         [forms/select-field selected [:period] (->> budgets/periods
                                                     (map name)
                                                     sort)]
         [forms/integer-field selected [:period-count]]]]
       [:div.card-footer
        [:button.btn.btn-primary {:on-click #(save-budget page-state)
                                  :title "Click here to save this budget."}
         (bs/icon-with-text :check "Save")]
        (html/space)
        [:button.btn.btn-light {:on-click #(swap! page-state dissoc :selected)}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- delete-budget-item
  [{:keys [account-id]} page-state]
  (when (js/confirm (str "Are you sure you want to remove the account "
                         (get-in @page-state [:accounts account-id :name])
                         " from the budget?"))
    (api/save (update-in (:detailed-budget @page-state)
                         [:items]
                         (fn [items]
                           (remove #(= (:account-id %)
                                       account-id)
                                   items)))
              #(swap! page-state assoc :detailed-budget %)
              (notify/danger-fn "Unable to delete the account from the budget: %s"))))

(defn- select-budget-item
  [item page-state]
  (let [total (decimal/sum (:periods item))
        average (/ total (-> item :periods count))
        mode (if (apply = (:periods item))
               :per-average
               :per-period)]
    (swap! page-state assoc
           :selected-item (assoc item
                                 :total total
                                 :average average
                                 ::entry-mode mode)))
  (html/set-focus "account-id"))

(defn- budget-item-row
  [item detail? page-state]
  ^{:key (str "budget-item-row-" (get-in item [:item :id]))}
  [:tr
   [:td (:caption item)]
   (when detail?
     (doall
      (map-indexed
       (fn [index value]
         ^{:key (str "period-value-" (:id item) "-" index)}
         [:td.text-right (format-decimal value)])
       (:periods (:item item)))))
   [:td.text-right (format-decimal (:total item))]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-info {:on-click #(select-budget-item (:item item) page-state)
                                   :title "Click here to edit this values for this account."}
      (bs/icon :pencil)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete-budget-item (:item item) page-state)
                                     :title "Click here to remove this account from the budget."}
      (bs/icon :x-circle)]]]])

(defn- budget-item-group-header-row
  [item-group detail?]
  ^{:key (str "budget-item-header-row-" (:caption item-group))}
  [:tr.report-header
   [:td (:caption item-group)]
   (when detail?
     (doall
      (map-indexed
       (fn [index _value]
         ^{:key (str "period-value-" (:caption item-group) "-" index)}
         [:td (html/space)])
       (:periods item-group))))
   [:td {:col-span 2} (html/space)]])

(defn- budget-item-group-footer-row
  [item-group detail?]
  ^{:key (str "budget-item-footer-row-" (:caption item-group))}
  [:tr.report-footer
   [:td (html/space)]
   (when detail?
     (doall
      (map-indexed
       (fn [index value]
         ^{:key (str "period-value-" (:caption item-group) "-" index)}
         [:td.text-right (format-decimal value)])
       (:periods item-group))))
   [:td.text-right (format-decimal (:total item-group))]
   [:td (html/space)]])

(defn- budget-item-rows
  [item-group detail? page-state]
  (concat [(budget-item-group-header-row item-group detail?)]
          (map #(budget-item-row % detail? page-state)
               (:items item-group))
          [(budget-item-group-footer-row item-group detail?)]))

(defn- budget-items-table
  [page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        accounts (r/cursor page-state [:accounts])
        rendered-budget (make-reaction (fn []
                                         (budgets/render @budget
                                                         #(get-in @accounts [%]))))
        selected-item (r/cursor page-state [:selected-item])
        detail-flag? (r/cursor page-state [:show-period-detail?])
        detail? (make-reaction #(and @detail-flag?
                                     (not @selected-item)))
        period-count (r/cursor page-state [:detailed-budget :period-count])]
    (fn []
      [:table.table.table-hover.table-borderless
       {:style (when-not @detail? {:width "20em"})}
       [:thead
        [:tr
         [:th "Account"]
         (when @detail?
           (doall
            (map (fn [index]
                   ^{:key (str "period-header-" index)}
                   [:th.text-right (budgets/period-description index @budget)])
                 (range @period-count))))
         [:th.text-right "Total"]
         [:th (html/space)]]]
       [:tbody
        (doall (mapcat #(budget-item-rows % @detail? page-state) @rendered-budget))]])))

(defn- period-row
  [index item budget]
  ^{:key (str "period-" index)}
  [:tr
   [:td (budgets/period-description index budget)]
   [:td [forms/decimal-input item [:periods index]]]])

(defn- apply-budget-item-updates
  [{:keys [average total] :as item} {:keys [period-count]}]
  (cond-> (select-keys item [:id :account-id :periods])
    (= :per-average (::entry-mode item))
    (assoc :periods (repeat period-count average))

    (= :per-total (::entry-mode item))
    (assoc :periods (repeat period-count (/ total period-count)))))

(defn- temp-id?
  [value]
  (not (integer? value)))

(defn- update-budget-item
  [budget item]
  (let [f (if (temp-id? (:id item))
            (fn [items]
              (conj (or items [])
                    (dissoc item :id)))
            (fn [items]
              (mapv #(if (= (:id item)
                            (:id %))
                       item
                       %)
                    items)))]
    (update-in budget [:items] f)))

(defn- save-budget-item
  [page-state]
  (let [budget (get-in @page-state [:detailed-budget])
        item (apply-budget-item-updates
              (get-in @page-state [:selected-item])
              budget)]
    (api/save (update-budget-item budget item)
              (fn [saved]
                (swap! page-state
                       #(-> %
                            (assoc :detailed-budget saved)
                            (dissoc :selected-item))))
              (notify/danger-fn "Unable to save the budget detail: %s"))))

(defn- period-fields-per-period
  [item budget]
  [:table.table
   [:thead
    [:tr
     [:th "Period"]
     [:th "Amount"]]]
   [:tbody
    (->> (range (:period-count budget))
         (map #(period-row % item budget))
         (doall))]])

(defn- period-fields-per-total
  [item _]
  [:div
   [forms/decimal-field item [:total] {:validate [:required]}]])

(defn- period-fields-per-average
  [item _]
  [forms/decimal-field item [:average] {:validate [:required]}])

(defn- period-fields
  [item page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        entry-mode (r/cursor item [::entry-mode])]
    (fn []
      [:div
       (bs/nav-tabs [{:caption "By Period"
                      :elem-key :per-period
                      :active? (= :per-period @entry-mode)
                      :on-click #(swap! item assoc ::entry-mode :per-period)}
                     {:caption "By Total"
                      :elem-key :per-total
                      :active? (= :per-total @entry-mode)
                      :on-click #(swap! item assoc ::entry-mode :per-total)}
                     {:caption "By Average"
                      :elem-key :per-average
                      :active? (= :per-average @entry-mode)
                      :on-click #(swap! item assoc ::entry-mode :per-average)}])
       [:div.mt-2
        [:form {:on-submit #(.preventDefault %)
                :no-validate true}
         (case @entry-mode
           :per-period
           [period-fields-per-period item @budget]
           :per-total
           (period-fields-per-total item @budget)
           :per-average
           (period-fields-per-average item @budget)

           [:div.alert.alert-danger
            (str "Unknown entry mode " @entry-mode)])]]])))

(defn- budget-item-form
  [page-state]
  (let [item (r/cursor page-state [:selected-item])
        accounts (r/cursor page-state [:accounts])]
    (fn []
      [:div
       [:form
        [forms/typeahead-field
         item
         [:account-id]
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @accounts
                              vals
                              (filter #(string/includes? (string/lower-case (:path %))
                                                         term))
                              callback)))
          :caption-fn :path
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> @accounts
                          vals
                          (filter #(= id (:id %)))
                          first
                          callback))}]
        [period-fields item page-state]]
       [:button.btn.btn-primary {:on-click #(save-budget-item page-state)
                                 :title "Click here to save this budget line item."}
        (bs/icon-with-text :check "Save")]
       (html/space)
       [:button.btn.btn-light {:on-click #(swap! page-state dissoc :selected-item)
                               :title "Click here to cancel this operation."}
        (bs/icon-with-text :x "Cancel")]])))

(defn- budget-details
  [page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        selected-item (r/cursor page-state [:selected-item])]
    (fn []
      [:div
       [:h1 (str "Budget Detail: " (:name budget))]
       [:div.row
        [:div.col
         [forms/checkbox-field page-state [:show-period-detail?]]]]
       [:div.row
        [:div.col
         [budget-items-table page-state]]
        (when @selected-item
          [:div.col
           [budget-item-form page-state]])]
       [:button.btn.btn-primary {:on-click #(swap! page-state assoc
                                                   :selected-item {:id (random-uuid)
                                                                   :periods (->> (range (:period-count @budget))
                                                                                 (map (constantly 0M))
                                                                                 (into []))
                                                                   ::entry-mode :per-total})
                                 :disabled (boolean @selected-item)
                                 :title "Click here to add a new budget line item"}
        (bs/icon-with-text :plus "Add")]
       (html/space)
       [:button.btn.btn-light {:on-click #(swap! page-state dissoc :detailed-budget)
                               :title "Click here to return to the list of budgets"}
        (bs/icon-with-text :arrow-left-short "Back")]])))

(defn- load-accounts
  [page-state]
  (accounts-api/select (fn [result]
                         (->> result
                              accounts/nest
                              accounts/unnest
                              (map (juxt :id identity))
                              (into {})
                              (swap! page-state assoc :accounts)))
                       (notify/danger-fn "Unable to load the accounts: %s")))

(defn- index []
  (let [page-state (r/atom {})
        selected (r/cursor page-state [:selected])
        detailed-budget (r/cursor page-state [:detailed-budget])]
    (load-budgets page-state)
    (load-accounts page-state)
    (fn []
      [:div.mt-5
       (if @detailed-budget
         [budget-details page-state]
         [:div
          [:h1 "Budgets"]
          [:div.row
           [:div.col-md-6
            [budgets-list page-state]]
           (when @selected
             [:div.col-md-6
              [budget-form page-state]])]])])))

(secretary/defroute "/budgets" []
  (swap! app-state assoc :page #'index))
