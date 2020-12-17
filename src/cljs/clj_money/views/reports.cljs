(ns clj-money.views.reports
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [clj-money.util :refer [format-decimal
                                    format-percent]]
            [clj-money.inflection :refer [humanize
                                          title-case]]
            [clj-money.bootstrap :as bs]
            [clj-money.state :refer [app-state]]
            [clj-money.forms :as forms]
            [clj-money.notifications :as notify]
            [clj-money.api.budgets :as bdt]
            [clj-money.api.reports :as rpt]))

(defmulti ^:private report-row :style)

(defmethod ^:private report-row :header
  [{:keys [caption value]} _]
  ^{:key (str "report-row-" caption)}
  [:tr.report-header
   [:th {:scope :row} caption]
   [:td.text-right (format-decimal value)]])

(defmethod ^:private report-row :data
  [{:keys [id caption value depth]} hide-zeros?]
  ^{:key (str "report-row-" id)}
  [:tr {:class (when (and hide-zeros?
                          (zero? value))
                 "d-none")}
   [:td
    [:span {:class (str "account-depth-" depth)}
     caption]]
   [:td.text-right
    [:span {:class (str "value-depth-" depth)} (format-decimal value)]]])

(defmethod ^:private report-row :summary
  [{:keys [caption value]} _]
  ^{:key (str "report-row-" caption)}
  [:tr.report-summary
   [:th {:scope :row} caption]
   [:td.text-right (format-decimal value)]])

(defn- start-of-year
  ([] (start-of-year (t/today)))
  ([date]
   (t/local-date (t/year date) 1 1)))

(defn- fetch-income-statement
  [page-state]
  (rpt/income-statement (select-keys (:income-statement @page-state) [:start-date :end-date])
                        (fn [result]
                          (swap! page-state #(-> %
                                                 (assoc-in [:income-statement :report] result)
                                                 (dissoc :loading?))))
                        (notify/danger-fn "Unable to fetch the report: %s")))

(defn- income-statement-filter
  [page-state]
  (let [loading? (r/cursor page-state [:loading?])]
    (fn []
      [:form.d-print-none {:on-submit (fn [e]
                                        (.preventDefault e)
                                        false)
                           :style {:max-width "785px"}}
       [:div.row
        [:div.col
         [forms/date-input
          page-state
          [:income-statement :start-date]
          {:placeholder "Start date"
           :validate [:required]}]]
        [:div.col
         [forms/date-input
          page-state
          [:income-statement :end-date]
          {:placeholder "End date"
           :validate [:required]}]]
        [:div.col
         [:button.btn.btn-primary {:on-click (fn []
                                               (swap! page-state assoc :loading? true)
                                               (fetch-income-statement page-state))
                                   :title "Click here to get the report with the specified parameters"}
          (if @loading?
            [:div.spinner-border.spinner-border-sm.text-light
             [:span.sr-only "Loading..."]]
            (bs/icon :arrow-repeat))]]]
       [:div.row
        [:div.col
         [forms/checkbox-field
          page-state
          [:hide-zeros?]
          {:caption "Hide Zero-Balance Accounts"}]]]])))

(defn- income-statement
  [page-state]
  (let [hide-zeros? (r/cursor page-state [:hide-zeros?])
        report (r/cursor page-state [:income-statement :report])]
    (fn []
      [:div
       [:h2 "Income Statement"]
       [income-statement-filter page-state]
       (when @report
         [:table.mt-3.table.table-hover.table-borderless.w-50
          [:tbody
           (doall (map #(report-row % @hide-zeros?) @report))]])])))

(defn- fetch-balance-sheet
  [page-state]
  (rpt/balance-sheet (select-keys (:balance-sheet @page-state) [:as-of])
                     (fn [result]
                       (swap! page-state #(-> %
                                              (assoc-in [:balance-sheet :report] result)
                                              (dissoc :loading?))))
                     (fn [error]
                       (swap! page-state dissoc :loading?)
                       (notify/danger (str "Unable to fetch the report: " (or (:message error) error))))))

(defn- balance-sheet-filter
  [page-state]
  (let [loading? (r/cursor page-state [:loading?])]
    (fn []
      [:form.d-print-none {:on-submit (fn [e]
                                        (.preventDefault e)
                                        false)
                           :style {:max-width "512px"}}
       [:div.row
        [:div.col
         [forms/date-input
          page-state
          [:balance-sheet :as-of]
          {:placeholder "As Of"
           :validate [:required]}]]
        [:div.col
         [:button.btn.btn-primary {:on-click (fn []
                                               (swap! page-state assoc :loading? true)
                                               (fetch-balance-sheet page-state))
                                   :title "Click here to get the report with the specified parameters"}
          (if @loading?
            [:div.spinner-border.spinner-border-sm.text-light
             [:span.sr-only "Loading..."]]
            (bs/icon :arrow-repeat))]]]
       [:div.row
        [:div.col
         [forms/checkbox-field
          page-state
          [:hide-zeros?]
          {:caption "Hide Zero-Balance Accounts"}]]]])))

(defn- balance-sheet
  [page-state]
  (let [hide-zeros? (r/cursor page-state [:hide-zeros?])
        report (r/cursor page-state [:balance-sheet :report])]
    (fn []
      [:div
       [:h2 "Balance Sheet"]
       [balance-sheet-filter page-state]
       (when @report
         [:table.mt-3.table.table-hover.table-borderless.w-75
          [:tbody
           (doall (map #(report-row % @hide-zeros?) @report))]])])))

(defn- fetch-budget-report
  [page-state]
  (rpt/budget (select-keys (:budget @page-state) [:budget-id])
              (fn [result]
                (swap! page-state #(-> %
                                       (assoc-in [:budget :report] result)
                                       (dissoc :loading?))))
              (notify/danger-fn "Unable to fetch the budget report: %s")))

(defn- budget-filter
  [page-state]
  (let [budgets (r/cursor page-state [:budget :budgets])
        loading? (r/cursor page-state [:loading?])
        options (make-reaction #(map (juxt :id :name) @budgets))]
    (fn []
      [:form.form-inline.d-print-none {:on-submit (fn [e]
                                                    (.preventDefault e)
                                                    false)}
       [forms/select-elem page-state [:budget :budget-id] options]
       [forms/integer-input page-state [:budget :depth] {:class "ml-sm-2"
                                                         :placeholder "Depth"
                                                         :style {:width "5em"}}]
       [:button.btn.btn-primary.ml-sm-2 {:on-click (fn []
                                                     (swap! page-state assoc :loading? true)
                                                     (fetch-budget-report page-state))
                                         :title "Click here to get the report with the specified parameters"}
        (if @loading?
          [:div.spinner-border.spinner-border-sm.text-light
           [:span.sr-only "Loading..."]]
          (bs/icon :arrow-repeat))]])))

(defn- budget-report-row
  [{:keys [id caption style budget actual difference percent-difference actual-per-period]}]
  ^{:key (str "report-row-" (or id caption))}
  [:tr {:class (str "report-" (name style))}
   [:td caption]
   [:td.text-right (format-decimal budget)]
   [:td.text-right (format-decimal actual)]
   [:td.text-right {:class (when (> 0 difference) "text-light bg-danger")} (format-decimal difference)]
   [:td.text-right (format-percent percent-difference)]
   [:td.text-right (format-decimal actual-per-period)]])

(defn- load-budgets
  [page-state]
  (bdt/search (fn [result]
                (swap! page-state
                       #(-> %
                            (assoc-in [:budget :budgets] result)
                            (assoc-in [:budget :budget-id] (:id (first result))))))
              (notify/danger-fn "Unable to load the budgets: %s")))

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

(defn- budget
  [page-state]
  (let [report (r/cursor page-state [:budget :report])
        depth (r/cursor page-state [:budget :depth])]
    (load-budgets page-state)
    (fn []
      [:div
       [:h2 "Budget"]
       [budget-filter page-state]
       (when @report
         [:div
          [:h2.mt-3 (:title @report)]
          [:table.mt-3.table.table-hover.table-borderless
           [:thead
            [:tr
             [:th "Account"]
             [:th.text-right "Budget"]
             [:th.text-right "Actual"]
             [:th.text-right "Diff"]
             [:th.text-right "% Diff"]
             [:th.text-right "Act/Mo"]]]
           [:tbody
            (->> (:items @report)
                 (refine-and-flatten @depth)
                 (map budget-report-row)
                 doall)]]])])))

(defn- load-portfolio
  [page-state]
  (swap! page-state assoc :loading? true)
  (let [{:keys [current-nav] :as state} (get-in @page-state [:portfolio])]
    (rpt/portfolio {:aggregate current-nav
                    :as-of (get-in state [:filter :as-of])}
                   (fn [result]
                     (swap! page-state #(-> %
                                            (assoc-in [:portfolio
                                                       current-nav
                                                       :report]
                                                      result)
                                            (dissoc :loading?))))
                   (notify/danger-fn "Unable to load the accounts report"))))

(defn- visible?
  [record visible-ids]
  (or (nil? (:parents record))
      (every? #(visible-ids %) (:parents record))))

(defn- toggle-visibility
  [state id]
  (update-in state
             [:portfolio (get-in state [:portfolio :current-nav]) :visible-ids]
             #(if (% id)
                (disj % id)
                (conj % id))))

(defn- format-shares
  [shares]
  (when shares
    (format-decimal shares {:fraction-digits 4})))

(defn- portfolio-report-row
  [{:keys [id
           parents
           style
           caption
           shares-owned
           shares-purchased
           cost-basis
           current-value
           gain-loss
           gain-loss-percent]
    :as record}
   visible-ids
   page-state]
  ^{:key (str "report-row-" (string/join "-" (cons id parents)))}
  [:tr {:class (cond-> [(str "report-" style)]
                 (not (visible? record visible-ids))
                 (conj "d-none"))
        :on-click (when-not (= "data" style)
                    #(swap! page-state toggle-visibility id))}
   [:td {:class (when (= "data" style)
                  "text-right")}
    caption]
   [:td.text-right (format-shares shares-purchased)]
   [:td.text-right (format-shares shares-owned)]
   [:td.text-right (format-decimal cost-basis)]
   [:td.text-right (format-decimal current-value)]
   [:td.text-right {:class (if (> 0 gain-loss)
                             "text-danger"
                             "text-success")}
    (format-decimal gain-loss)]
   [:td.text-right {:class (if (> 0 gain-loss)
                             "text-danger"
                             "text-success")}
    (format-percent gain-loss-percent)]])

(defn- render-portfolio
  [page-state]
  (let [current-nav (r/cursor page-state [:portfolio :current-nav])
        report (make-reaction #(get-in @page-state [:portfolio @current-nav :report]))
        visible-ids (make-reaction #(get-in @page-state [:portfolio @current-nav :visible-ids]))]
    (fn []
      [:table.table.table-hover.table-borderless.portfolio
       [:thead
        [:tr
         [:th "Purchase Date"]
         [:th.text-right "Shares Purchased"]
         [:th.text-right "Shares Owned"]
         [:th.text-right "Cost Basis"]
         [:th.text-right "Current Value"]
         [:th.text-right "Gain/Loss"]
         [:th.text-right "G/L %"]]]
       [:tbody
        (cond
          (nil? @report)
          [:tr [:td.inline-status {:col-span 4} "Loading..."]]

          (seq @report)
          (doall (map #(portfolio-report-row % @visible-ids page-state) @report))

          :else
          [:tr [:td.inline-status {:col-span 4} "No investment accounts found."]])]])))

(defn- portfolio
  [page-state]
  (let [current-nav (r/cursor page-state [:portfolio :current-nav])
        report-filter (r/cursor page-state [:portfolio :filter])
        loading? (r/cursor page-state [:loading?])]
    (load-portfolio page-state)
    (fn []
      [:div
       [:div.row
        [:div.col
         (bs/nav-pills (map (fn [id]
                              {:elem-key id
                               :caption (humanize id)
                               :on-click (fn []
                                           (reset! current-nav id)
                                           (load-portfolio page-state))
                               :active? (= id @current-nav)})
                            [:by-account :by-commodity]))]
        [:div.col
         [forms/date-input report-filter [:as-of]]]
        [:div.col
         [:button.btn.btn-info {:on-click (fn []
                                            (load-portfolio page-state))
                                :title "Click here to refresh the report."}
          (if @loading?
            [:div.spinner-border.spinner-border-sm.text-light
             [:span.sr-only "Loading..."]]
            (bs/icon :arrow-repeat))]]]
       [:div.mt-2
        [render-portfolio page-state]]])))

(defn- index []
  (let [page-state (r/atom {:selected :income-statement
                            :hide-zeros? true
                            :income-statement {:start-date (start-of-year)
                                               :end-date (t/today)}
                            :balance-sheet {:as-of (t/today)}
                            :budget {:depth 0}
                            :portfolio {:current-nav :by-account
                                        :filter {:as-of (t/today)}
                                        :by-account {:visible-ids #{}}
                                        :by-commodity {:visible-ids #{}}}})
        selected (r/cursor page-state [:selected])]
    (fn []
      [:div.mt-5
       [:h1.d-print-none "Reports"]
       (bs/nav-tabs {:class "d-print-none"}
                    (map (fn [id]
                           {:elem-key id
                            :caption (title-case (humanize id))
                            :active? (= id @selected)
                            :on-click #(swap! page-state assoc :selected id)})
                         [:income-statement
                          :balance-sheet
                          :budget
                          :portfolio]))
       [:div.mt-3
        (case @selected
          :income-statement [income-statement page-state]
          :balance-sheet [balance-sheet page-state]
          :budget [budget page-state]
          :portfolio [portfolio page-state])]])))

(secretary/defroute "/reports" []
  (swap! app-state assoc :page #'index))
