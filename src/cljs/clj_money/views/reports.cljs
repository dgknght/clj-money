(ns clj-money.views.reports
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [clj-money.util :refer [format-decimal
                                    format-percent]]
            [clj-money.inflection :refer [humanize
                                          title-case]]
            [clj-money.bootstrap :as bs]
            [clj-money.state :refer [app-state]]
            [clj-money.plain-forms :as forms]
            [clj-money.notifications :as notify]
            [clj-money.api.budgets :as bdt]
            [clj-money.api.reports :as rpt]))

(defmulti ^:private report-row :style)

(defmethod ^:private report-row :header
  [{:keys [caption value]}]
  ^{:key (str "report-row-" caption)}
  [:tr.report-header
   [:th {:scope :row} caption]
   [:td.text-right (format-decimal value)]])

(defmethod ^:private report-row :data
  [{:keys [id caption value depth]}]
  ^{:key (str "report-row-" id)}
  [:tr
   [:td
    [:span {:class (str "account-depth-" depth)} caption]]
   [:td.text-right
    [:span {:class (str "value-depth-" depth)} (format-decimal value)]]])

(defmethod ^:private report-row :summary
  [{:keys [caption value]}]
  ^{:key (str "report-row-" caption)}
  [:tr.report-summary
   [:th {:scope :row} caption]
   [:td.text-right (format-decimal value)]])

(defn- start-of-year
  ([] (start-of-year (t/today)))
  ([date]
   (t/local-date (t/year date) 1 1)))

(defn- fetch-income-statement
  [ctl-state]
  (rpt/income-statement (select-keys @ctl-state [:start-date :end-date])
                        (fn [result]
                          (swap! ctl-state #(-> %
                                                (assoc :report result)
                                                (dissoc :loading?))))
                        (notify/danger-fn "Unable to fetch the report: %s")))

(defn- income-statement-filter
  [ctl-state]
  (let [loading? (r/cursor ctl-state [:loading?])]
    (fn []
      [:form.form-inline.d-print-none {:on-submit (fn [e]
                                                    (.preventDefault e)
                                                    false)}
       [forms/date-input ctl-state [:start-date] {:placeholder "Start date"
                                                  :class "mr-sm-2"
                                                  :validate [:required]}]
       [forms/date-input ctl-state [:end-date] {:placeholder "End date"
                                                :class "mr-sm-2"
                                                :validate [:required]}]
       [:button.btn.btn-primary {:on-click (fn []
                                             (swap! ctl-state assoc :loading? true)
                                             (fetch-income-statement ctl-state))
                                 :title "Click here to get the report with the specified parameters"}
        (if @loading?
          [:div.spinner-border.spinner-border-sm.text-light
           [:span.sr-only "Loading..."]]
          (bs/icon :arrow-repeat))]])))

(defn- income-statement []
  (let [ctl-state (r/atom {:start-date (start-of-year)
                           :end-date (t/today)})
        report (r/cursor ctl-state [:report])]
    (fn []
      [:div
       [:h2 "Income Statement"]
       [income-statement-filter ctl-state]
       (when @report
         [:table.mt-3.table.table-hover.table-borderless.w-50
          [:tbody
           (doall (map report-row @report))]])])))

(defn- fetch-balance-sheet
  [ctl-state]
  (rpt/balance-sheet (select-keys @ctl-state [:as-of])
                     (fn [result]
                       (swap! ctl-state #(-> %
                                             (assoc :report result)
                                             (dissoc :loading?))))
                     (fn [error]
                       (swap! ctl-state dissoc :loading?)
                       (notify/danger (str "Unable to fetch the report: " (or (:message error) error))))))

(defn- balance-sheet-filter
  [ctl-state]
  (let [loading? (r/cursor ctl-state [:loading?])]
    (fn []
      [:form.form-inline.d-print-none {:on-submit (fn [e]
                                                    (.preventDefault e)
                                                    false)}
       [forms/date-input ctl-state [:as-of] {:placeholder "As Of"
                                                  :class "mr-sm-2"
                                                  :validate [:required]}]
       [:button.btn.btn-primary {:on-click (fn []
                                             (swap! ctl-state assoc :loading? true)
                                             (fetch-balance-sheet ctl-state))
                                 :title "Click here to get the report with the specified parameters"}
        (if @loading?
          [:div.spinner-border.spinner-border-sm.text-light
           [:span.sr-only "Loading..."]]
          (bs/icon :arrow-repeat))]])))

(defn- balance-sheet []
  (let [ctl-state (r/atom {:as-of (t/today)})
        report (r/cursor ctl-state [:report])]
    (fn []
      [:div
       [:h2 "Balance Sheet"]
       [balance-sheet-filter ctl-state]
       (when @report
         [:table.mt-3.table.table-hover.table-borderless.w-75
          [:tbody
           (doall (map report-row @report))]])])))

(defn- fetch-budget-report
  [ctl-state]
  (rpt/budget (select-keys @ctl-state [:budget-id])
              (fn [result]
                (swap! ctl-state #(-> %
                                      (assoc :report result)
                                      (dissoc :loading?))))
              (notify/danger-fn "Unable to fetch the budget report: %s")))

(defn- budget-filter
  [ctl-state]
  (let [budgets (r/cursor ctl-state [:budgets])
        loading? (r/cursor ctl-state [:loading?])
        options (make-reaction #(map (juxt :id :name) @budgets))]
    (fn []
      [:form.form-inline.d-print-none {:on-submit (fn [e]
                                                    (.preventDefault e)
                                                    false)}
       [forms/select-elem ctl-state [:budget-id] options]
       [forms/integer-input ctl-state [:depth] {:class "ml-sm-2"
                                                :placeholder "Depth"
                                                :style {:width "5em"}}]
       [:button.btn.btn-primary.ml-sm-2 {:on-click (fn []
                                                     (swap! ctl-state assoc :loading? true)
                                                     (fetch-budget-report ctl-state))
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
  [ctl-state]
  (bdt/search (fn [result]
                (swap! ctl-state #(assoc %
                                         :budgets result
                                         :budget-id (:id (first result)))))
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

(defn- budget []
  (let [ctl-state (r/atom {:as-of (t/today)
                           :depth 0})
        report (r/cursor ctl-state [:report])
        depth (r/cursor ctl-state [:depth])]
    (load-budgets ctl-state)
    (fn []
      [:div
       [budget-filter ctl-state]
       (when @report
         [:div
          [:h2.mt-3 (str "Budget " (:title @report))]
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

(defn- index []
  (let [page-state (r/atom {:selected :income-statement})
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
                          :budget]))
       [:div.mt-3
        (case @selected
          :income-statement [income-statement]
          :balance-sheet [balance-sheet]
          :budget [budget])]])))

(secretary/defroute "/reports" []
  (swap! app-state assoc :page #'index))
