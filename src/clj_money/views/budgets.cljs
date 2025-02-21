(ns clj-money.views.budgets
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [cljs-time.periodic :refer [periodic-seq]]
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
            [clj-money.api.transaction-items :as tran-items]
            [clj-money.api.budgets :as api]))

(defn- post-load-budgets
  [page-state budgets]
  (-busy)
  (swap! page-state assoc :budgets budgets))

(defn- load-budgets
  [page-state]
  (+busy)
  (api/search (map (partial post-load-budgets page-state))))

(defn- post-delete-budget
  [page-state]
  (-busy)
  (load-budgets page-state))

(defn- delete-budget
  [budget page-state]
  (when (js/confirm (str "Are you sure you want to delete the budget " (:name budget) "?"))
    (+busy)
    (api/delete budget
                (map (partial post-delete-budget page-state)))))

(defn- post-load-budget-details
  [page-state budget]
  (-busy)
  (swap! page-state assoc :detailed-budget budget)
  (set-focus "account-id"))

(defn- load-budget-details
  [budget page-state]
  (+busy)
  (api/find (:id budget)
            (map (partial post-load-budget-details page-state))))

(defn- budget-row
  [budget page-state]
  ^{:key (str "budget-row-" (:id budget))}
  [:tr
   [:td (:name budget)]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-light {:on-click (fn []
                                               (swap! page-state assoc :selected budget)
                                               (set-focus "name"))
                                   :title "Click here to edit this budget"}
      (icon :pencil :size :small)]
     [:button.btn.btn-sm.btn-light {:on-click #(load-budget-details budget page-state)
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
                                           {:period :month
                                            :period-count 12})
                                    (set-focus "name"))
                        :disabled @busy?}
                 :icon :plus
                 :caption "Add"}]]])))

(defn- post-save-budget
  [page-state _budget]
  (-busy)
  (load-budgets page-state)
  (swap! page-state dissoc :selected))

(defn- save-budget
  [page-state]
  (+busy)
  (api/save (dissoc (:selected @page-state) :items)
           (map (partial post-save-budget page-state)) ))

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
         [forms/text-field selected [:name] {:validations #{::v/required}}]
         [forms/date-field selected [:start-date] {:validations #{::v/required}}]
         [forms/select-field selected [:period] (->> budgets/periods
                                                     (map name)
                                                     sort)]
         [forms/integer-field selected [:period-count] {:validations #{::v/required}}]
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

(defn- post-delete-budget-item
  [page-state budget]
  (-busy)
  (swap! page-state assoc :detailed-budget budget))

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
              (map (partial post-delete-budget-item page-state)))))

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
  [item detail? page-state]
  ^{:key (str "budget-item-row-" (get-in item [:item :id]))}
  [:tr.budget-item-row
   [:th
    [:span.d-none.d-md-inline (:caption item)]
    [:span.d-md-none (abbreviate (:caption item))]]
   (when detail?
     (doall
      (map-indexed
       (fn [index value]
         ^{:key (str "period-value-" (:id item) "-" index)}
         [:td.text-end (format-decimal value)])
       (:periods (:item item)))))
   [:td.text-end (format-decimal (:total item))]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-light {:on-click #(select-budget-item (:item item) page-state)
                                   :title "Click here to edit this values for this account."}
      (icon :pencil :size :small)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete-budget-item (:item item) page-state)
                                     :title "Click here to remove this account from the budget."}
      (icon :x-circle :size :small)]]]])

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
         [:td.text-end (format-decimal value)])
       (:periods item-group))))
   [:td.text-end (format-decimal (:total item-group))]
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
         [:td.text-end (format-decimal value)])
       (:periods item-group))))
   [:td.text-end (format-decimal (:total item-group))]
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
        rendered-budget (make-reaction (fn []
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
                          (map (fn [periods]
                                 (callback (map (comp #(round % round-to)
                                                      :quantity)
                                                periods))))))
  (repeat period-count 0M))

(defn- update-budget-item
  [budget item]
  (let [f (if (util/temp-id? (:id item))
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

(defn- post-save-budget-item
  [page-state budget]
  (-busy)
  (swap! page-state #(-> %
                         (assoc :detailed-budget budget)
                         (dissoc :selected-item))))

(defn- save-budget-item
  [page-state]
  (+busy)
  (let [{budget :detailed-budget
         item :selected-item
         periods :calculated-periods} @page-state
        item (-> item
                 (select-keys [:id :account-id :spec])
                 (assoc :periods periods))]
    (api/save (update-budget-item budget item)
              (map (partial post-save-budget-item page-state)))))

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
  ^{:key "period-field-total"}
  [forms/decimal-field item [:spec :total] {:validate [:required]}])

(defn- period-fields-per-average
  [item]
  ^{:key "period-field-average"}
  [forms/decimal-field item [:spec :average] {:validate [:required]}])

(defn- period-fields-weekly
  [item]
  [:<>
  ^{:key "period-field-amount-per"}
   [forms/decimal-field item [:spec :amount-per] {:validate [:required]}]
  ^{:key "period-field-start-date"}
   [forms/date-field item [:spec :start-date] {:validate [:required]}]
  ^{:key "period-field-week-count"}
   [forms/integer-field item [:spec :week-count] {:validate [:required]}]])

(defn- period-fields-historical
  [item]
  [:<>
  ^{:key "period-field-start-date"}
   [forms/date-field item [:spec :start-date] {:validate [:required]}]
  ^{:key "period-field-round-to"}
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

(defn- filtered-period-field-nav-options
  [budget]
  (filter (fn [{:keys [filter-fn]
                :or {filter-fn (constantly true)}}]
            (filter-fn budget))
          period-nav-options))

(defn- period-field-nav-items
  [current-mode item budget]
  (map (fn [{:keys [elem-key] :as option}]
         (assoc option
                :active? (= elem-key current-mode)
                :on-click #(swap! item assoc-in [:spec :entry-mode] elem-key)))
       (filtered-period-field-nav-options budget)))

(defn- period-field-select-options
  [_current-mode _item budget]
  (map (fn [{:keys [elem-key caption]}]
         [elem-key caption])
       (filtered-period-field-nav-options budget)))

(defn- period-fields
  [item page-state]
  (let [budget (r/cursor page-state [:detailed-budget])
        entry-mode (r/cursor item [:spec :entry-mode])]
    (fn []
      [:div.card
       [:div.card-header
        [:h4 "Values"]]
       [:div.card-body
        (bs/nav-tabs {:class "d-none d-md-flex"} (period-field-nav-items @entry-mode item @budget))
        [:div.d-md-none
         [forms/select-elem
          item
          [:spec :entry-mode]
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
           :weekly
           (period-fields-weekly item)
           :historical
           (period-fields-historical item)

           [:div.alert.alert-danger
            (str "Unknown entry mode " @entry-mode)])]]])))

(defn- budget-item-form
  [page-state]
  (let [item (r/cursor page-state [:selected-item])]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (save-budget-item page-state))}
       [forms/typeahead-field
        item
        [:account-id]
        {:search-fn (fn [input callback]
                      (callback (accounts/find-by-path input @accounts)))
         :caption-fn #(string/join "/" (:path %))
         :value-fn :id
         :find-fn (fn [id callback]
                    (->> @accounts
                         (filter #(= id (:id %)))
                         first
                         callback))}]
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
                          :on-click #(swap! page-state assoc
                                            :selected-item {:id (random-uuid)
                                                            :periods (->> (range (:period-count @budget))
                                                                          (map (constantly 0M))
                                                                          (into []))
                                                            :spec {:entry-mode :per-total}})
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
