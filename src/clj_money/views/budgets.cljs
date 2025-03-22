(ns clj-money.views.budgets
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.calendar :as cal]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.util :as util]
            [clj-money.components :refer [button]]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id
                                     +busy
                                     -busy
                                     busy?]]
            [clj-money.budgets :as budgets]
            [clj-money.accounts :as accounts]
            [clj-money.api.transaction-items :as trx-items]
            [clj-money.api.budgets :as api]))

(defn- load-budgets
  [page-state]
  (+busy)
  (api/search {}
              :callback -busy
              :on-success #(swap! page-state assoc :budgets %)))

(defn- delete-budget
  [budget page-state]
  (when (js/confirm (str "Are you sure you want to delete the budget " (:name budget) "?"))
    (+busy)
    (api/delete budget
                :callback -busy
                :on-success #(load-budgets page-state))))

(defn- load-budget-details
  [budget page-state]
  (+busy)
  (api/find (:id budget)
            :callback -busy
            :on-success (fn [b]
                          (swap! page-state assoc :detailed-budget b)
                          (set-focus "account-id"))))

(defn- budget-row
  [budget page-state]
  ^{:key (str "budget-row-" (:id budget))}
  [:tr
   [:td (:budget/name budget)]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-secondary {:on-click (fn []
                                               (swap! page-state assoc :selected budget)
                                               (set-focus "name"))
                                   :title "Click here to edit this budget"}
      (icon :pencil :size :small)]
     [:button.btn.btn-sm.btn-secondary {:on-click #(load-budget-details budget page-state)
                                   :title "Click here to fill out details for this budget."}
      (icon :collection :size :small)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete-budget budget page-state)
                                     :title "Click here to remove this budget."}
      (icon :x-circle :size :small)]]]])

(defn- budgets-table
  [page-state]
  (let [budgets (r/cursor page-state [:budgets])]
    (fn []
      [:table.table.table-hover
       [:thead
        [:tr
         [:th.w-75 "Name"]
         [:th.w-25 (html/space)]]]
       [:tbody
        (if budgets
          (if (seq @budgets)
            (doall (map #(budget-row % page-state) @budgets))
            [:tr
             [:td {:col-span 2} [:span.inline-status "No budgets."]]])
          [:tr
           [:td {:col-span 2} [:span.inline-status "Loading..."]]])]])))

(defn- budgets-list
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        details (r/cursor page-state [:detailed-budget])
        hide? (make-reaction #(or @selected @details))]
    (fn []
      [:div {:class (when @hide? "d-none")}
       [budgets-table page-state]
       [:div.mt-2
        [button {:html {:class "btn-primary"
                        :title "Click here to add a new budget."
                        :on-click (fn []
                                    (swap! page-state
                                           assoc
                                           :selected
                                           #:budget{:period :month
                                                    :period-count 12})
                                    (set-focus "name"))}
                 :icon :plus
                 :disabled busy?
                 :caption "Add"}]]])))

(defn- save-budget
  [page-state]
  (+busy)
  (-> (:selected @page-state)
      (dissoc :budget/items)
      (update-in [:budget/period] keyword)
      (util/pp-> ::to-save)
      (api/save :callback -busy
                :on-success (fn []
                              (load-budgets page-state)
                              (swap! page-state dissoc :selected)))))

(defn- budget-form
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        auto-create (r/cursor selected [:auto-create-items])]
    (fn []
      (when @selected
        [:form {:on-submit (fn [e]
                             (.preventDefault e)
                             (v/validate selected)
                             (when (v/valid? selected)
                               (save-budget page-state)))
                :no-validate true}
         [forms/text-field selected [:budget/name] {:validations #{::v/required}}]
         [forms/date-field selected [:budget/start-date] {:validations #{::v/required}}]
         [forms/select-field selected [:budget/period] (->> budgets/periods
                                                            (map name)
                                                            sort)]
         [forms/integer-field selected [:budget/period-count] {:validations #{::v/required}}]
         [forms/checkbox-field selected [:auto-create-items]]
         [forms/date-field selected [:auto-create-start-date] {:disabled-fn #(not @auto-create)}]
         [:div.mt-3
          [button {:html {:class "btn-primary"
                          :type :submit
                          :title "Click here to save this budget."}
                   :icon :check
                   :caption "Save"}]
          [button {:html {:class "btn-secondary ms-2"
                          :type :button
                          :title "Click here to cancel this operation."
                          :on-click #(swap! page-state dissoc :selected)}
                   :icon :x
                   :caption "Cancel"}]]]))))

(defn- delete-budget-item
  [{:keys [account-id]} page-state]
  (when (js/confirm (str "Are you sure you want to remove the account "
                         (get-in @accounts-by-id [account-id :name])
                         " from the budget?"))
    (+busy)
    (api/save (update-in (:detailed-budget @page-state)
                         [:items]
                         (fn [items]
                           (remove #(= (:account-id %)
                                       account-id)
                                   items)))
              :callback -busy
              :on-success #(swap! page-state assoc :detailed-budget %))))

(defn- infer-spec
  [{:budget-item/keys [periods]}]
  (if (apply = periods)
    {:entry-mode :per-average
     :average (first periods)}
    {:entry-mode :per-period}))

(defn- derive-entry-mode
  [spec]
  (let [conformed (s/conform ::budgets/item-spec spec)]
    (if (= :ss/invalid? conformed)
      :per-period
      (first conformed))))

(defn- ensure-spec
  [{:as item :budget-item/keys [spec]}]
  (if (:budget-item/spec item)
    (assoc item :entry-mode (derive-entry-mode spec))
    (assoc item :budget-item/spec (infer-spec item))))

(defn- select-budget-item
  [item page-state]
  (swap! page-state assoc :selected-item (ensure-spec item))
  (set-focus "account-id"))

(defn- abbreviate
  [account-name]
  (if (<= (count account-name) 10)
    account-name
    (let [segments (string/split account-name #"/")]
      (string/join
          "/"
          (concat (map #(str (first %))
                       (butlast segments))
                  (take-last 1 segments))))))

(defn- budget-item-row
  [{:budget-section/keys [caption total item]} detail? page-state]
  ^{:key (str "budget-item-row-" (:id item))}
  [:tr.budget-item-row
   [:th
    [:span.d-none.d-md-inline caption]
    [:span.d-md-none (abbreviate caption)]]
   (when detail?
     (doall
      (map-indexed
       (fn [index value]
         ^{:key (str "period-value-" (:id item) "-" index)}
         [:td.text-end (format-decimal value)])
       (:budget-item/periods item))))
   [:td.text-end (format-decimal total)]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-secondary {:on-click #(select-budget-item item page-state)
                                   :title "Click here to edit this values for this account."}
      (icon :pencil :size :small)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete-budget-item item page-state)
                                     :title "Click here to remove this account from the budget."}
      (icon :x-circle :size :small)]]]])

(defn- budget-section-header-row
  [{:budget-section/keys [caption periods]} detail?]
  ^{:key (str "budget-section-header-row-" caption)}
  [:tr.report-header
   [:th {:col-span 2 :scope "row"} caption]
   (when detail?
     (doall
      (map-indexed
       (fn [index _value]
         ^{:key (str "period-value-" caption "-" index)}
         [:td (html/space)])
       periods)))
   [:td {:col-span 2} (html/space)]])

(defn- budget-section-footer-row
  [{:budget-section/keys [caption total periods]} detail?]
  ^{:key (str "budget-item-footer-row-" caption)}
  [:tr.report-footer
   [:td (html/space)]
   (when detail?
     (doall
      (map-indexed
       (fn [index value]
         ^{:key (str "period-value-" caption "-" index)}
         [:td.text-end (format-decimal value)])
       periods)))
   [:td.text-end (format-decimal total)]
   [:td (html/space)]])

(defn- budget-item-condensed-row
  [{:budget-section/keys [caption periods total]} detail?]
  ^{:key (str "budget-summary-row-" caption)}
  [:tr.report-condensed-summary
   [:th {:scope "row"} caption]
   (when detail?
     (doall
      (map-indexed
       (fn [index value]
         ^{:key (str "period-value-" caption "-" index)}
         [:td.text-end (format-decimal value)])
       periods)))
   [:td.text-end (format-decimal total)]
   [:td (html/space)]])

(defn- budget-section-rows
  [{:as budget-section :budget-section/keys [items]} detail? page-state]
  (if (seq items)
    (concat [(budget-section-header-row budget-section detail?)]
            (map #(budget-item-row % detail? page-state)
                 items)
            [(budget-section-footer-row budget-section detail?)])
    [(budget-item-condensed-row budget-section detail?)]))

(defn- budget-items-table
  [page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        rendered-budget (make-reaction
                          (fn []
                            (when @accounts-by-id
                              (budgets/render @budget
                                              {:find-account @accounts-by-id
                                               :tags [:tax :mandatory :discretionary]})))) ; TODO: make this user editable
        selected-item (r/cursor page-state [:selected-item])
        detail-flag? (r/cursor page-state [:show-period-detail?])
        detail? (make-reaction #(and @detail-flag?
                                     (not @selected-item)))
        period-count (r/cursor page-state [:detailed-budget :period-count])]
    (fn []
      [:table.table.table-hover.table-borderless
       {:style (when-not @detail? {:max-width "20em"})}
       [:thead
        [:tr
         [:th "Account"]
         (when @detail?
           (doall
             (map (fn [index]
                    ^{:key (str "period-header-" index)}
                    [:th.text-end (budgets/period-description index @budget)])
                  (range @period-count))))
         [:th.text-end "Total"]
         [:th (html/space)]]]
       [:tbody
        (->> @rendered-budget
             (mapcat #(budget-section-rows % @detail? page-state))
             doall)]])))

(defn- post-save-budget-item
  [page-state]
  (fn [budget]
    (swap! page-state #(-> %
                           (assoc :detailed-budget budget)
                           (dissoc :selected-item)))))

(defn- save-budget-item
  [page-state]
  (+busy)
  (let [{budget :detailed-budget
         item :selected-item
         periods :calculated-periods} @page-state
        item (-> item
                 (select-keys [:id
                               :budget-item/account
                               :budget-item/spec])
                 (assoc :budget-item/periods periods))]
    (-> budget
        (update-in [:budget/items] #(util/upsert-into item {:sort-key (comp :account/name :budget-item/account)} %))
        (api/save :callback -busy
                  :on-success (post-save-budget-item page-state)))))

(defn- period-row
  [index item budget]
  ^{:key (str "period-" index)}
  [:tr
   [:td (budgets/period-description index budget)]
   [:td [forms/decimal-input item [:budget-item/periods index]]]])

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
     [:td (format-decimal (reduce decimal/+ (:budget-item/periods @item)))]]]
   [:tbody
    (->> (range (:period-count budget))
         (map #(period-row % item budget))
         (doall))]])

(defn- period-fields-per-total
  [item]
  ^{:key "period-field-total"}
  [forms/decimal-field item [:budget-item/spec :total] {:validate [:required]}])

(defn- period-fields-per-average
  [item]
  ^{:key "period-field-average"}
  [forms/decimal-field item [:budget-item/spec :average] {:validate [:required]}])

(defn- period-fields-weekly
  [item]
  [:<>
  ^{:key "period-field-amount-per"}
   [forms/decimal-field item [:budget-item/spec :amount] {:validate [:required]}]
  ^{:key "period-field-start-date"}
   [forms/date-field item [:budget-item/spec :start-date] {:validate [:required]}]
  ^{:key "period-field-week-count"}
   [forms/integer-field item [:budget-item/spec :week-count] {:validate [:required]}]])

(defn- period-fields-historical
  [item]
  [:<>
  ^{:key "period-field-start-date"}
   [forms/date-field item [:budget-item/spec :start-date] {:validate [:required]}]
  ^{:key "period-field-round-to"}
   [forms/integer-field item [:budget-item/spec :round-to]]])

(defn- recalc-periods
  [i b periods]
  (let [{:budget-item/keys [spec] :as item} @i
        budget @b]
    (when (and (s/valid? ::budgets/item-spec spec)
               budget)
      (a/go
        (let [updated (a/<! (budgets/calc-periods
                              item
                              budget
                              :fetch-item-summaries trx-items/summarize))]
          (reset! periods updated))))))

(defn- periods-table
  [page-state]
  (let [item (r/cursor page-state [:selected-item])
        budget (r/cursor page-state [:detailed-budget])
        periods (r/cursor page-state [:calculated-periods])
        calculated (r/track! recalc-periods item budget periods)
        total (make-reaction #(reduce decimal/+ @periods))]
    (fn []
      [:table.table.table-sm.table-hover {:title (str "periods for item" (:account-id @item))}
       [:thead
        [:tr
         [:th "Month"]
         [:th "Amount"]]]
       [:tfoot
        [:tr
         [:td
          (html/comment @calculated)] ; only need this rendered to trigger the reagent magic
         [:td.text-end (format-decimal @total)]]]
       [:tbody
        (->> @periods
             (map-indexed (fn [i amount]
                            ^{:key (str "weekly-budget-item-" i)}
                            [:tr
                             [:td
                              (nth cal/month-names i)] ; TODO: this assumes the budgets starts in January
                             [:td.text-end (format-decimal amount)]]))
             doall)]])))

(def ^:private period-nav-options
  [{:label "By Period"
    :id :per-period}
   {:label "By Total"
    :id :per-total}
   {:label "By Average"
    :id :per-average}
   {:label "Weekly"
    :id :per-week
    :filter-fn #(#{:month} (:budget/period %))}
   {:label "Historical"
    :id :historical}])

(defn- filtered-period-field-nav-options
  [budget]
  (filter (fn [{:keys [filter-fn]
                :or {filter-fn (constantly true)}}]
            (filter-fn budget))
          period-nav-options))

(defn- period-field-nav-items
  [current-mode item budget]
  (map (fn [{:keys [id] :as option}]
         (assoc option
                :active? (= id current-mode)
                :nav-fn #(swap! item assoc-in [:entry-mode] id)))
       (filtered-period-field-nav-options budget)))

(defn- period-field-select-options
  [_current-mode _item budget]
  (map (fn [{:keys [id label]}]
         [id label])
       (filtered-period-field-nav-options budget)))

(defn- period-fields
  [item page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        entry-mode (r/cursor item [:entry-mode])]
    (fn []
      [:div.card
       [:div.card-header
        [:h4 "Values"]]
       [:div.card-body
        (bs/nav-tabs {:class "d-none d-md-flex"} (period-field-nav-items @entry-mode item @budget))
        [:div.d-md-none
         [forms/select-elem
          item
          [:entry-mode]
          (period-field-select-options @entry-mode item @budget)
          {:transform-fn keyword}]]
        [:div.mt-2
         (case @entry-mode
           :per-period
           [period-fields-per-period item @budget]
           :per-total
           (period-fields-per-total item)
           :per-average
           (period-fields-per-average item)
           :per-week
           (period-fields-weekly item)
           :historical
           (period-fields-historical item)

           [:div.alert.alert-danger
            (str "Unknown entry mode " @entry-mode)])]]])))

(defn- budget-item-form
  [page-state]
  (let [item (r/cursor page-state [:selected-item])
        existing-items (r/cursor page-state [:detailed-budget :budget/items])
        exists? (make-reaction (fn []
                                 (->> @existing-items
                                      (remove #(util/id= @item %))
                                      (map (comp :id
                                                 :budget-item/account))
                                      set)))]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (save-budget-item page-state))}
       [forms/typeahead-field
        item
        [:budget-item/account :id]
        {:search-fn (fn [input callback]
                      (->> (accounts/find-by-path input @accounts)
                           (remove (comp @exists? :id))
                           callback))
         :caption-fn #(string/join "/" (:account/path %))
         :caption "Account"
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback @accounts-by-id id))}]
       [period-fields item page-state]
       [:div.mt-3
        [button {:html {:class "btn-primary"
                        :type :submit
                        :title "Click here to save this budget line item."}
                 :icon :check
                 :caption "Save"}]
        [button {:html {:class "btn-secondary ms-2"
                        :on-click #(swap! page-state dissoc :selected-item)
                        :type :button
                        :title "Click here to cancel this operation."}
                 :icon :x
                 :caption "Cancel"}]]])))

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
                                                       (get-in @selected-item [:entry-mode]))))
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
      [:div.h-100 {:class (when-not @budget "d-none")}
       [:div.d-none.d-lg-block
        [forms/checkbox-field page-state [:show-period-detail?]]]
       [:div.row.h-100
        [:div#budget-items-parent.col-md-6.h-100 {:class (when @selected-item "d-none")}
         [:div {:style {:max-height (str @scrollable-height "px")
                        :overflow-y "scroll"}}
          [budget-items-table page-state]]
         [:div.my-2
          [button {:html {:class "btn-primary"
                          :on-click (fn []
                                      (swap! page-state
                                             assoc
                                             :selected-item {:id (util/temp-id)
                                                             :budget-item/periods (->> (range (:period-count @budget))
                                                                                       (map (constantly 0M))
                                                                                       (into []))
                                                             :entry-mode :per-total})
                                      (set-focus "account-id"))
                          :disabled (or @busy?
                                        (boolean @selected-item))
                          :title "Click here to add a new budget line item"}
                   :icon :plus
                   :caption "Add"}]

          [button {:html {:class "btn-secondary ms-2"
                                  :on-click #(swap! page-state dissoc :detailed-budget)
                                  :disabled @busy?
                                  :title "Click here to return to the list of budgets"}
                           :icon :arrow-left-short
                           :caption "Back"}]]]
        (when @selected-item
          [:div.col-md-6
           [budget-item-form page-state]])
        (when @selected-item
          [:div.col-md-6
           (when @show-periods-table?
             [:div.mt-md-0.mt-2
              [periods-table page-state]])])]])))

(defn- index []
  (let [page-state (r/atom {})
        selected (r/cursor page-state [:selected])
        detailed-budget (r/cursor page-state [:detailed-budget])]
    (load-budgets page-state)
    (add-watch current-entity ::index (fn [& _] (load-budgets page-state)))
    (fn []
      [:div.mt-3
       [:div
        [:div.d-flex.justify-content-between
         [:h1 "Budgets"]
         (when @selected
           [:h2(if (:id @selected)
                 "Edit"
                 "New")])
         (when @detailed-budget
           [:h2 (:name @detailed-budget)])]
        [budget-details page-state]
        [:div.row
         [:div.col-md-6
          [budgets-list page-state]]
         [:div.col-md-6
          [budget-form page-state]]]]])))

(secretary/defroute "/budgets" []
  (swap! app-state assoc :page #'index))
