(ns clj-money.views.budgets
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [cljs-time.periodic :refer [periodic-seq]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.calendar :as cal]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.forms :as forms]
            [clj-money.state :refer [app-state]]
            [clj-money.bootstrap :as bs]
            [clj-money.budgets :as budgets]
            [clj-money.accounts :as accounts]
            [clj-money.api.transaction-items :as tran-items]
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
  (let [selected (r/cursor page-state [:selected])
        auto-create (r/cursor selected [:auto-create-items])]
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
         [forms/integer-field selected [:period-count]]
         [forms/checkbox-field selected [:auto-create-items]]
         [forms/date-field selected [:auto-create-start-date] {:disabled-fn #(not @auto-create)}]]]
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

(defn- infer-spec
  [item]
  (let [total (decimal/sum (:periods item))]
    (if (apply = (:periods item))
      {:entry-mode :per-average
       :average (/ total (-> item :periods count))}
      {:entry-mode :per-period})))

(defn- ensure-spec
  [item]
  (if (:spec item)
    item
    (assoc item :spec (infer-spec item))))

(defn- select-budget-item
  [item page-state]
  (swap! page-state assoc :selected-item (ensure-spec item))
  (html/set-focus "account-id"))

(defn- budget-item-row
  [item detail? page-state]
  ^{:key (str "budget-item-row-" (get-in item [:item :id]))}
  [:tr.budget-item-row
   [:th {:scope "row"} (:caption item)]
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
   [:th {:col-span 2 :scope "row"} (:caption item-group)]
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

(defn- budget-item-condensed-row
  [item-group detail?]
  ^{:key (str "budget-summary-row-" (:caption item-group))}
  [:tr.report-condensed-summary
   [:th {:scope "row"} (:caption item-group)]
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
  (if (seq (:items item-group))
    (concat [(budget-item-group-header-row item-group detail?)]
            (map #(budget-item-row % detail? page-state)
                 (:items item-group))
            [(budget-item-group-footer-row item-group detail?)])
    [(budget-item-condensed-row item-group detail?)]))

(defn- budget-items-table
  [page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        accounts (r/cursor page-state [:accounts])
        rendered-budget (make-reaction (fn []
                                         (budgets/render @budget
                                                         {:find-account @accounts
                                                          :tags [:tax :mandatory :discretionary]}))) ; TODO: make this user editable
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
        (->> @rendered-budget
             (mapcat #(budget-item-rows % @detail? page-state))
             doall)]])))

(defmulti calc-periods
  (fn [item _budget _callback]
    (get-in item [:spec :entry-mode])))

(defmethod calc-periods :per-period
  [item _ callback]
  (callback (:periods item)))

(defmethod calc-periods :per-average
  [{{:keys [average]} :spec} {:keys [period-count]} callback]
  (callback (repeat period-count average)))

(defmethod calc-periods :per-total
  [{{:keys [total]} :spec} {:keys [period-count]} callback]
  (callback (repeat period-count (/ total period-count))))

(defmethod calc-periods :weekly
  [{{:keys [start-date week-count amount-per]} :spec}
   {:keys [end-date period-count]}
   callback]
  (callback
    (if (and start-date week-count amount-per)
      (->> (periodic-seq start-date
                         (t/weeks week-count))
           (take-while #(t/before? % end-date))
           (group-by t/month)
           (map (comp (partial decimal/* amount-per)
                      count
                      second)))
      (repeat period-count 0M))))

(defn- round
  [value round-to]
  (if round-to
    (decimal/*
      (decimal/round
        (decimal// value
                   round-to))
      round-to)
    value))

(defmethod calc-periods :historical
  [{:keys [account-id]
    {:keys [start-date round-to]} :spec}
   {:keys [period-count period]}
   callback]
  (when start-date
    (tran-items/summarize {:transaction-date [start-date
                                              (t/minus (t/plus start-date
                                                               ((case period
                                                                  :month t/months
                                                                  :week t/weeks)
                                                                period-count))
                                                       (t/days 1))]
                           :account-id account-id
                           :interval-type period
                           :interval-count 1}
                          (fn [periods]
                            (callback (map (comp #(round % round-to)
                                                 :quantity)
                                           periods)))
                          (notify/danger-fn "Unable to get the historical transaction data: %s")))
  (repeat period-count 0M))

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
  (let [{budget :detailed-budget
         item :selected-item
         periods :calculated-periods} @page-state
        item (-> item
                 (select-keys [:id :account-id :spec])
                 (assoc :periods periods))]
    (api/save (update-budget-item budget item)
              (fn [saved]
                (swap! page-state
                       #(-> %
                            (assoc :detailed-budget saved)
                            (dissoc :selected-item))))
              (notify/danger-fn "Unable to save the budget detail: %s"))))

(defn- period-row
  [index item budget]
  ^{:key (str "period-" index)}
  [:tr
   [:td (budgets/period-description index budget)]
   [:td [forms/decimal-input item [:periods index]]]])

(defn- period-fields-per-period
  [item budget]
  [:table.table
   [:thead
    [:tr
     [:th "Period"]
     [:th "Amount"]]]
   [:tfoot
    [:tr
     [:td (html/space)]
     [:td (format-decimal (reduce decimal/+ (:periods @item)))]]]
   [:tbody
    (->> (range (:period-count budget))
         (map #(period-row % item budget))
         (doall))]])

(defn- period-fields-per-total
  [item]
  [:div
   [forms/decimal-field item [:spec :total] {:validate [:required]}]])

(defn- period-fields-per-average
  [item]
  [forms/decimal-field item [:spec :average] {:validate [:required]}])

(defn- period-fields-weekly
  [item]
  [:div
   [forms/decimal-field item [:spec :amount-per] {:validate [:required]}]
   [forms/date-field item [:spec :start-date] {:validate [:required]}]
   [forms/integer-field item [:spec :week-count] {:validate [:required]}]])

(defn- period-fields-historical
  [item]
  [:div
   [forms/date-field item [:spec :start-date] {:validate [:required]}]
   [forms/integer-field item [:spec :round-to]]])

(defn- deref-and-calc-periods
  [item budget periods]
  (when (and @item @budget)
    (calc-periods @item @budget #(reset! periods %))))

(defn- periods-table
  [page-state]
  (let [item (r/cursor page-state [:selected-item])
        budget (r/cursor page-state [:detailed-budget])
        periods (r/cursor page-state [:calculated-periods])
        calculated (r/track! deref-and-calc-periods item budget periods)
        total (make-reaction #(reduce decimal/+ @periods))]
    (fn []
      [:table.table.table-hover {:title (str "periods for item" (:account-id @item))}
       [:thead
        [:tr
         [:th "Month"]
         [:th "Amount"]]]
       [:tfoot
        [:tr
         [:td
          (html/comment @calculated)] ; only need this rendered to trigger the reagent magic
         [:td.text-right (format-decimal @total)]]]
       [:tbody
        (->> @periods
             (map-indexed (fn [i amount]
                            ^{:key (str "weekly-budget-item-" i)}
                            [:tr
                             [:td
                              (nth cal/month-names i)] ; TODO: this assumes the budgets starts in January
                             [:td.text-right (format-decimal amount)]]))
             doall)]])))

(def ^:private period-nav-options
  [{:caption "By Period"
    :elem-key :per-period}
   {:caption "By Total"
    :elem-key :per-total}
   {:caption "By Average"
    :elem-key :per-average}
   {:caption "Weekly"
    :elem-key :weekly
    :filter-fn #(#{:month} (:period %))}
   {:caption "Historical"
    :elem-key :historical}])

(defn- period-field-nav-items
  [current-mode item budget]
  (->> period-nav-options
       (filter (fn [{:keys [filter-fn]
                     :or {filter-fn (constantly true)}}]
                 (filter-fn budget)))
       (map (fn [{:keys [elem-key] :as option}]
              (assoc option
                     :active? (= elem-key current-mode)
                     :on-click #(swap! item assoc-in [:spec :entry-mode] elem-key))))))

(defn- period-fields
  [item page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        entry-mode (r/cursor item [:spec :entry-mode])]
    (fn []
      [:div
       (bs/nav-tabs (period-field-nav-items @entry-mode item @budget))
       [:div.mt-2
        (case @entry-mode
          :per-period
          [period-fields-per-period item @budget]
          :per-total
          (period-fields-per-total item)
          :per-average
          (period-fields-per-average item)
          :weekly
          (period-fields-weekly item)
          :historical
          (period-fields-historical item)

          [:div.alert.alert-danger
           (str "Unknown entry mode " @entry-mode)])]])))

(defn- budget-item-form
  [page-state]
  (let [item (r/cursor page-state [:selected-item])
        accounts (r/cursor page-state [:accounts])]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (save-budget-item page-state))}
       [forms/typeahead-field
        item
        [:account-id]
        {:search-fn (fn [input callback]
                      (callback (accounts/find-by-path input (vals @accounts))))
         :caption-fn #(string/join "/" (:path %))
         :value-fn :id
         :find-fn (fn [id callback]
                    (->> @accounts
                         vals
                         (filter #(= id (:id %)))
                         first
                         callback))}]
       [period-fields item page-state]
       [:button.btn.btn-primary {:type :submit
                                 :title "Click here to save this budget line item."}
        (bs/icon-with-text :check "Save")]
       (html/space)
       [:button.btn.btn-light {:on-click #(swap! page-state dissoc :selected-item)
                               :type :button
                               :title "Click here to cancel this operation."}
        (bs/icon-with-text :x "Cancel")]])))

(defonce resize-state (r/atom {}))

(defn- capture-window-height
  [& _]
  (swap! resize-state assoc :window-height (.-innerHeight js/window)))

(defn- budget-details
  [page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        selected-item (r/cursor page-state [:selected-item])
        show-periods-table? (make-reaction #(and @selected-item
                                                 (not= :per-period
                                                       (get-in @selected-item [:spec :entry-mode]))))
        window-height (r/cursor resize-state [:window-height])
        scrollable-height (make-reaction #(if (< @window-height 800)
                                            400
                                            (- @window-height 300)))]
    (when-not @window-height
      (capture-window-height))
    (when-not (:event-registered? @resize-state)
      (.addEventListener js/window
                         "resize"
                         capture-window-height))
    (fn []
      [:div.h-100
       [:h1 (str "Budget Detail: " (:name @budget))]
       [:div.row
        [:div.col
         [forms/checkbox-field page-state [:show-period-detail?]]]]
       [:div.row.h-100
        [:div#budget-items-parent.col.h-100 {:class (when @selected-item "d-none")}
         [:div {:style {:max-height (str @scrollable-height "px")
                        :overflow-y "scroll"}}
          [budget-items-table page-state]]
         [:div.my-2
          [:button.btn.btn-primary {:on-click #(swap! page-state assoc
                                                      :selected-item {:id (random-uuid)
                                                                      :periods (->> (range (:period-count @budget))
                                                                                    (map (constantly 0M))
                                                                                    (into []))
                                                                      :spec {:entry-mode :per-total}})
                                    :disabled (boolean @selected-item)
                                    :title "Click here to add a new budget line item"}
           (bs/icon-with-text :plus "Add")]
          (html/space)
          [:button.btn.btn-light {:on-click #(swap! page-state dissoc :detailed-budget)
                                  :title "Click here to return to the list of budgets"}
           (bs/icon-with-text :arrow-left-short "Back")]]]
        (when @selected-item
          [:div.col
           [budget-item-form page-state]])
        (when @selected-item
          [:div.col
           (when @show-periods-table?
             [periods-table page-state])])]])))

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
           [:div.col
            [budgets-list page-state]]
           (when @selected
             [:div.col
              [budget-form page-state]])]])])))

(secretary/defroute "/budgets" []
  (swap! app-state assoc :page #'index))
