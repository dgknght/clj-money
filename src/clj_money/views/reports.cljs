(ns clj-money.views.reports
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [dgknght.app-lib.models :refer [map-index]]
            [dgknght.app-lib.web :refer [format-decimal
                                         format-percent
                                         format-date]]
            [dgknght.app-lib.inflection :refer [humanize
                                                title-case]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.budgets :refer [period-description]]
            [clj-money.api.budgets :as bdt]
            [clj-money.api.reports :as rpt]))

(defmulti ^:private report-row :report/style)

(defmethod ^:private report-row :header
  [{:report/keys [caption value]} _]
  ^{:key (str "report-row-" caption)}
  [:tr.report-header
   [:th {:scope :row} caption]
   [:td.text-end (format-decimal value)]])

(defmethod ^:private report-row :data
  [{:report/keys [id caption value depth]} hide-zeros?]
  ^{:key (str "report-row-" (or id caption))}
  [:tr {:class (when (and hide-zeros?
                          (zero? value))
                 "d-none")}
   [:td
    [:span {:class (str "account-depth-" depth)}
     caption]]
   [:td.text-end
    [:span {:class (str "value-depth-" depth)} (format-decimal value)]]])

(defmethod ^:private report-row :summary
  [{:report/keys [caption value]} _]
  ^{:key (str "report-row-" caption)}
  [:tr.report-summary
   [:th {:scope :row} caption]
   [:td.text-end (format-decimal value)]])

(defn- start-of-year
  ([] (start-of-year (t/today)))
  ([date]
   (t/local-date (t/year date) 1 1)))

(defmulti load-report #(:selected (deref %)))

(defmethod load-report :income-statement
  [page-state]
  (+busy)
  (swap! page-state update-in [:income-statement] dissoc :report)
  (let [report-spec (get-in @page-state [:income-statement :options])]
    (rpt/income-statement report-spec
                          :callback -busy
                          :on-success #(swap! page-state
                                              assoc-in
                                              [:income-statement :report]
                                              %))))

(defn- income-statement-options
  [options]
  [:<>
   [forms/date-field
    options
    [:start-date]
    {:placeholder "Start date"
     :validate [:required]}]
   [forms/date-field
    options
    [:end-date]
    {:placeholder "End date"
     :validate [:required]}]
   [forms/checkbox-field
    options
    [:hide-zeros?]
    {:caption "Hide Zero-Balance Accounts"}]])

(defn- income-statement-header
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :income-statement @selected))
        start-date (r/cursor page-state [:income-statement :start-date])
        end-date (r/cursor page-state [:income-statement :end-date])]
    (fn []
      [:span {:class (when @hide? "d-none")}
       (format-date @start-date)
       " - "
       (format-date @end-date)])))

(defn- income-statement
  [page-state]
  (let [hide-zeros? (r/cursor page-state [:income-statement :options :hide-zeros?])
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :income-statement @selected))
        report (r/cursor page-state [:income-statement :report])]
    (fn []
      [:div.row
       [:div.col-md-6.offset-md-3
        [:table.mt-3.table.table-hover.table-borderless {:class (when @hide? "d-none")}
         [:tbody
          (if @report
            (doall (map #(report-row % @hide-zeros?) @report))
            [:tr
             [:td.text-center
              [bs/spinner]]])]]]])))

(defmethod load-report :balance-sheet
  [page-state]
  (+busy)
  (swap! page-state update-in [:balance-sheet] dissoc :report)
  (rpt/balance-sheet (get-in @page-state [:balance-sheet :options])
                     :callback -busy
                     :on-success #(swap! page-state assoc-in [:balance-sheet :report] %)))

(defn- balance-sheet-options
  [options]
  [:<>
   [forms/date-field
    options
    [:as-of]
    {:placeholder "As Of"
     :validate [:required]}]
   [forms/checkbox-field
    options
    [:hide-zeros?]
    {:caption "Hide Zero-Balance Accounts"}]])

(defn- balance-sheet-header
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :balance-sheet @selected))
        as-of (r/cursor page-state [:balance-sheet :as-of])]
    (fn []
      [:span {:class (when @hide? "d-none")}
       (format-date @as-of)])))

(defn- balance-sheet
  [page-state]
  (let [hide-zeros? (r/cursor page-state [:balance-sheet :options :hide-zeros?])
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :balance-sheet @selected))
        report (r/cursor page-state [:balance-sheet :report])]
    (fn []
      [:div.row
       [:div.col-md-6.offset-md-3
        [:table.mt-3.table.table-hover.table-borderless {:class (when @hide? "d-none")}
         [:tbody
          (if @report
            (doall (map #(report-row % @hide-zeros?) @report))
            [:tr
             [:td.text-center
              [bs/spinner]]])]]]])))

(defn- receive-budget-report
  [page-state]
  (fn [report]
    (swap! page-state
           #(-> %
                (assoc-in [:budget :report] report)
                (update-in [:budget] dissoc :apply-info)))))

(defmethod load-report :budget
  [page-state]
  (+busy)
  (swap! page-state update-in [:budget] dissoc :report)
  (rpt/budget (dissoc (get-in @page-state [:budget :options]) :depth)
              :callback -busy
              :on-success (receive-budget-report page-state)))

(defn- budget-options
  [options page-state]
  (let [budgets (r/cursor page-state [:budgets])
        budget-items (make-reaction #(->> (vals @budgets)
                                          (sort-by :start-date t/after?)
                                          (map (juxt :id :name))))]
    (fn []
      (when (seq @budget-items)
        [:<>
         [forms/select-field options [:budget-id] budget-items]
         [forms/integer-field options [:depth] {:class "ms-sm-2"
                                                :placeholder "Depth"
                                                :style {:width "5em"}}]]))))

(defn- receive-budget
  [budget
   {account-id :id
    :keys [actual-per-period]
    :as report-item}
   page-state]
  (-busy)
  (let [budget (update-in budget
                          [:items]
                          #(map-index :account-id %))
        budget-item (or (get-in budget [:items account-id])
                        {:account-id account-id
                         :periods []})
        periods (vec
                  (repeat (:period-count budget)
                          actual-per-period))]
    (swap! page-state
           assoc-in [:budget :apply-info] {:budget budget
                                           :budget-item (assoc budget-item :periods periods)
                                           :report-item report-item})))

(defn- apply-to-budget
  [report-item page-state]
  (+busy)
  (let [{{:keys [budget-id]} :budget} @page-state]
    (bdt/find budget-id
              :callback -busy
              :on-success #(receive-budget % report-item page-state))))

(defn- budget-report-row
  [{:report/keys [id
                  caption
                  style
                  budget
                  actual
                  difference
                  percent-difference
                  actual-per-period]
    :as item}
   page-state]
  ^{:key (str "report-row-" (or id caption))}
  [:tr {:class (str "report-" (name style))}
   [:td caption]
   [:td.text-end.d-none.d-md-table-cell (format-decimal budget)]
   [:td.text-end.d-none.d-md-table-cell (format-decimal actual)]
   [:td.d-flex.justify-content-between {:class (when (> 0 difference) "text-light bg-danger")}
    (when (= :data style)
      [:span.d-print-none.d-none.d-md-inline
       {:on-click #(apply-to-budget item page-state)
        :title "Click here to update the budget with recorded actual values."
        :style {:cursor :pointer}}
       (icon :gear :size :small)])
    [:span.flex-fill.text-end (format-decimal difference)]]
   [:td.text-end.d-none.d-md-table-cell (format-percent percent-difference)]
   [:td.text-end.d-none.d-md-table-cell (format-decimal actual-per-period)]])

(defn- receive-budgets
  [page-state]
  (fn [budgets]
    (swap! page-state
           (fn [state]
             (-> state
                 (assoc-in [:budgets] (->> budgets
                                           (map (juxt :id identity))
                                           (into {})))
                 (assoc-in [:budget :options :budget-id] (-> budgets first :id)))))))

(defn- load-budgets
  [page-state]
  (+busy)
  (bdt/search {}
              :callback -busy
              :on-success (receive-budgets page-state)))

(defn- refine-items
  [depth items]
  (->> items
       (remove #(< depth (:depth %)))
       (map #(if (= depth (:depth %))
               (merge % (:roll-up %))
               %))
       (remove #(= 0 (:actual %) (:budget %)))
       (sort-by :difference)))

(defn- refine-and-flatten
  [depth groups]
  (mapcat #(concat [%]
                   (refine-items depth (:items %)))
          groups))

(defn- save-budget
  [page-state]
  (let [{:keys [budget budget-item]} (get-in @page-state [:budget :apply-info])]
    (+busy)
    (bdt/save (update-in budget [:items] (fn [items]
                                           (-> items
                                               (assoc (:account-id budget-item)
                                                      (update-in budget-item
                                                                 [:periods]
                                                                 #(mapv (fnil identity (decimal/zero))
                                                                        %)))
                                               vals)))
              :callback -busy
              :on-success #(load-report page-state))))

(defn- apply-budget-item-form
  [page-state]
  (let [apply-info (r/cursor page-state [:budget :apply-info])
        report-item (r/cursor apply-info [:report-item])
        account (r/cursor report-item [:account])
        budget (r/cursor apply-info [:budget])
        original-budget-item (make-reaction #(get-in @budget [:items (:id @account)]))
        budget-item (r/cursor apply-info [:budget-item])
        original-total (make-reaction
                         #(decimal/sum (:periods @original-budget-item)))
        item-total (make-reaction
                     #(decimal/sum (:periods @budget-item)))]
    (fn []
      [:div.background.fixed-top {:class (when-not @apply-info "d-none")}
       [:div.card
        [:div.card-header (str "Apply Budget Item: " (string/join "/" (:path @account)))]
        [:div.card-body
         [:form {:on-submit #(.preventDefault %)
                 :no-validate true}
          [:table.table.table-hover
           [:thead
            [:tr
             [:th "Period"]
             [:th.text-end "Current"]
             [:th "New"]]]
           [:tbody
            (->> (range (:period-count @budget))
                 (map-indexed (fn [index _]
                                ^{:key (str "budget-item-period-" index)}
                                [:tr
                                 [:td (period-description index @budget)]
                                 [:td.text-end (format-decimal
                                                 (get-in @original-budget-item
                                                         [:periods index]))]
                                 [:td [forms/decimal-input budget-item [:periods index]]]]))
                 doall)]
           [:tfoot
            [:tr
             [:td.text-end {:col-span 2} (format-decimal @original-total)]
             [:td.text-end (format-decimal @item-total)]]]]]]
        [:div.card-footer
         [:button.btn-primary {:title "Click here to save this budget item."
                               :on-click #(save-budget page-state)}
          (icon-with-text :check "Save")]
         [:button.btn-secondary.ms-2
          {:title "Click here to cancel this edit."
           :on-click #(swap! page-state update-in [:budget] dissoc :apply-info)}
          (icon-with-text :x "Cancel")]]]])))

(defn- budget-header
  [page-state]
  (let [budget-id (r/cursor page-state [:budget :options :budget-id])
        budgets (r/cursor page-state [:budgets])
        budget (make-reaction (fn []
                                (->> @budgets
                                     (filter #(= @budget-id (:id %)))
                                     first)))
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :budget @selected))]
    (fn []
      [:span {:class (when @hide? "d-none")} (:name budget)])))

(defn- budget
  [page-state]
  (let [report (r/cursor page-state [:budget :report])
        depth (r/cursor page-state [:budget :options :depth])
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :budget @selected))]
    (load-budgets page-state)
    (fn []
      [:<>
       [apply-budget-item-form page-state]
       [:table.mt-3.table.table-hover.table-borderless {:class (when @hide? "d-none")}
        [:thead
         [:tr
          [:th "Account"]
          [:th.text-end.d-none.d-md-table-cell "Budget"]
          [:th.text-end.d-none.d-md-table-cell "Actual"]
          [:th.text-end "Diff"]
          [:th.text-end.d-none.d-md-table-cell "% Diff"]
          [:th.text-end.d-none.d-md-table-cell "Act/Mo"]]]
        [:tbody
         (if @report
           (->> (:items @report)
                (refine-and-flatten @depth)
                (map (comp
                       #(budget-report-row % page-state)
                       #(assoc % :account (get-in @accounts-by-id [(:id %)]))))
                doall)
           [:tr
            [:td.text-center {:col-span 6}
             [bs/spinner]]])]]])))

(defmethod load-report :portfolio
  [page-state]
  (+busy)
  (let [opts (get-in @page-state [:portfolio :options])]
    (swap! page-state update-in [:portfolio] dissoc :report)
    (rpt/portfolio (:filter opts)
                   :callback -busy
                   :on-success #(swap! page-state
                                       assoc-in
                                       [:portfolio :report]
                                       %))))

(defn- visible?
  [record visible-ids]
  (or (nil? (:parents record))
      (every? #(visible-ids %) (:parents record))))

(defn- toggle-visibility
  [state id]
  (let [aggregate (get-in state [:portfolio
                                 :options
                                 :filter
                                 :aggregate])]
    (update-in state
               [:portfolio (get-in state [:portfolio
                                          :options
                                          aggregate])
                :visible-ids]
               (fn [ids]
                 (if (ids id)
                   (disj ids id)
                   (conj ids id))))))

(defn- format-shares
  [shares]
  (when shares
    (format-decimal shares {:fraction-digits 4})))

(defn- portfolio-report-row
  [{:report/keys [style
                  depth
                  caption
                  shares-owned
                  shares-purchased
                  cost-basis
                  current-value
                  gain-loss
                  gain-loss-percent]
    :keys [id]
    :as record}
   visible-ids
   page-state]
  ^{:key (str "report-row-" caption)}
  [:tr {:class (cond-> [(str "report-" style)]
                 (not (visible? record visible-ids))
                 (conj "d-none"))
        :on-click (when-not (= "data" style)
                    #(swap! page-state toggle-visibility id))}
   [:td {:class [(when (= "data" style)
                  "text-end")
                 (str "account-depth-" depth)]}
    caption]
   [:td.text-end.d-none.d-md-table-cell (format-shares shares-purchased)]
   [:td.text-end.d-none.d-md-table-cell (format-shares shares-owned)]
   [:td.text-end.d-none.d-md-table-cell (format-decimal cost-basis)]
   [:td.text-end (format-decimal current-value)]
   [:td.text-end {:class (if (> 0 gain-loss)
                           "text-danger"
                           "text-success")}
    (format-decimal gain-loss)]
   [:td.text-end.d-none.d-md-table-cell {:class (if (> 0 gain-loss)
                                                  "text-danger"
                                                  "text-success")}
    (format-percent gain-loss-percent)]])

(defn- render-portfolio
  [page-state]
  (let [report (r/cursor page-state [:portfolio :report])
        aggregate (r/cursor page-state [:portfolio :options :filter :aggregate])
        visible-ids (make-reaction #(get-in @page-state [:portfolio :options @aggregate :visible-ids]))]
    (fn []
      [:table.table.table-hover.table-borderless.portfolio
       [:thead
        [:tr
         [:th "Purchase Date"]
         [:th.text-end.d-none.d-md-table-cell "Shares Purchased"]
         [:th.text-end.d-none.d-md-table-cell "Shares Owned"]
         [:th.text-end.d-none.d-md-table-cell "Cost Basis"]
         [:th.text-end "Current Value"]
         [:th.text-end "Gain/Loss"]
         [:th.text-end.d-none.d-md-table-cell "G/L %"]]]
       [:tbody
        (cond
          (nil? @report)
          [:tr [:td.text-center {:col-span 7} (bs/spinner)]]

          (seq @report)
          (doall (map #(portfolio-report-row % @visible-ids page-state) @report))

          :else
          [:tr [:td.inline-status {:col-span 7} "No investment accounts found."]])]])))

(defn- portfolio-options
  [options page-state]
  (let [current-nav (r/cursor page-state [:portfolio :options :filter :aggregate])]
    (fn []
      [:<>
       (bs/nav-pills {:class "mb-2"} (map (fn [id]
                                            {:id id
                                             :label (humanize id)
                                             :nav-fn (fn []
                                                       (reset! current-nav id)
                                                       (load-report page-state))
                                             :active? (= id @current-nav)})
                                          [:by-account :by-commodity]))
       [forms/date-field options [:as-of]]])))

(defn- portfolio
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :portfolio @selected))]
    (fn []
      [:div.mt-2 {:class (when @hide? "d-none")}
       [render-portfolio page-state]])))

(def ^:private report-types
  [:balance-sheet
   :income-statement
   :budget
   :portfolio])

(defn- report-nav-item-fn
  [page-state]
  (fn [id]
    (let [selected (get-in @page-state [:selected])]
      {:id id
       :label (title-case (humanize id))
       :active? (= id selected)
       :nav-fn (fn []
                 (swap! page-state assoc :selected id)
                 (when-not (get-in @page-state [id :report])
                   (load-report page-state)))})))

(defn- filter-form
  [page-state]
  (fn []
    (let [selected (r/cursor page-state [:selected])
          options (r/cursor page-state [@selected :options])]
      [:div#report-options.offcanvas.offcanvas-end {:tab-index -1}
       [:div.offcanvas-header.d-flex.justify-content-between
        [:h3 "Options"]
        [:button.btn-close.text-reset {:data-bs-dismiss :offcanvas}]]
       [:div.offcanvas-body
        [:form {:no-validate true
                :on-submit (fn [e]
                             (.preventDefault e)
                             (v/validate options)
                             (when (v/valid? options)
                               (load-report page-state)))}
         (case @selected
           :income-statement (income-statement-options options)
           :balance-sheet    (balance-sheet-options options)
           :budget           [budget-options options page-state]
           :portfolio        [portfolio-options options page-state])
         [:div.mt-3
          [:button.btn.btn-primary
           {:type :submit
            :data-bs-dismiss :offcanvas
            :title "Click here to show the report with the specified parameters"}
           (icon-with-text :arrow-repeat "Show")]]]]])))

(defn- init-state []
  (r/atom {:selected :balance-sheet
           :income-statement {:options
                              {:start-date (start-of-year)
                               :end-date (t/today)
                               :hide-zeros? true}}
           :balance-sheet {:options {:as-of (t/today)
                                     :hide-zeros? true}}
           :budget {:options {:depth 1
                              :tags [:tax :mandatory :discretionary]}} ; TODO: make this user editable
           :portfolio {:options {:filter {:aggregate :by-account
                                          :as-of (t/today)}
                                 :by-account {:visible-ids #{}}
                                 :by-commodity {:visible-ids #{}}}}}))

(defn- index []
  (let [page-state (init-state)
        selected (r/cursor page-state [:selected])]
    (load-report page-state)
    (add-watch current-entity
               ::index
               (fn [_ _ _ entity]
                 (when entity
                   (load-report page-state))))
    (fn []
      [:div.mt-3

       [:div.d-print-none.d-flex.justify-content-between
        [:h1 "Reports"]
        [:button.btn.btn-dark
         {:type :button
          :data-bs-toggle "offcanvas"
          :data-bs-target "#report-options"
          :aria-controls "report-options" }
         (icon :gear)]]

       [:div.d-none.d-print-block.text-center
        [:h1 (humanize @selected)]
        [:h2
         [income-statement-header page-state]
         [balance-sheet-header page-state]
         [budget-header page-state]]]
       [:div.d-print-none
        ; small screen nav
        [:div.d-md-none.mt-2
         [forms/select-elem
          page-state
          [:selected]
          (map #(vector % (humanize %)) report-types)
          {:transform-fn keyword
           :on-change (fn [s field]
                        (when-not (get-in @page-state [(get-in @s field) :report])
                          (load-report page-state)))}]]
        ; big screen nav
        (bs/nav-tabs {:class "d-none d-md-flex"}
                     (map (report-nav-item-fn page-state)
                          report-types))]
       [filter-form page-state] 
       [:div.mt-3
        [income-statement page-state]
        [balance-sheet page-state]
        [budget page-state]
        [portfolio page-state]]])))

(secretary/defroute "/reports" []
  (swap! app-state assoc :page #'index))
