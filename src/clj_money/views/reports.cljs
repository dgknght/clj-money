(ns clj-money.views.reports
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.web :refer [format-decimal
                                         format-percent
                                         format-date]]
            [dgknght.app-lib.inflection :refer [humanize
                                                title-case]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.dom :as dom]
            [clj-money.util :refer [id=]]
            [clj-money.decimal :as d]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts-by-id
                                     busy?
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
  [{:report/keys [caption value depth] :keys [id]} hide-zeros?]
  ^{:key (str "report-row-" (or id caption))}
  [:tr {:class (when (and hide-zeros?
                          (d/zero? value))
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

(defn- apply-depth
  [depth records]
  (if (some? depth)
    (remove #(and (= :data (:report/style %))
                  (> (:report/depth %) depth))
            records)
    records))

(defn- report-max-depth
  [records]
  (if (seq records)
    (->> records
         (filter #(= :data (:report/style %)))
         (map :report/depth)
         (reduce max 0)
         inc)
    9))

(defn- budget-max-depth
  [report]
  (report-max-depth (mapcat :report/items (:items report))))

(defmulti load-report #(:selected (deref %)))

(defmethod load-report :income-statement
  [page-state]
  (+busy)
  (swap! page-state update-in [:income-statement] dissoc :report)
  (let [report-spec (get-in @page-state [:income-statement :options])]
    (rpt/income-statement report-spec
                          :callback -busy
                          :on-success (fn [report]
                                        (let [max-d (report-max-depth report)]
                                          (swap! page-state
                                                 #(-> %
                                                      (assoc-in [:income-statement :report] report)
                                                      (assoc-in [:income-statement :options :depth] (dec max-d)))))))))

(defn- income-statement-option-fields
  [page-state]
  (let [options (r/cursor page-state [:income-statement :options])
        report (r/cursor page-state [:income-statement :report])
        depth (r/cursor options [:depth])
        max-depth (make-reaction #(report-max-depth @report))]
    (fn []
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
       [:div.mt-3
        [:button.btn.btn-primary
         {:type :submit
          :data-bs-dismiss :offcanvas
          :title "Click here to show the report with the specified parameters"}
         (icon-with-text :arrow-repeat "Show")]]
       [:hr]
       [forms/checkbox-field
        options
        [:hide-zeros?]
        {:caption "Hide Zero-Balance Accounts"}]
       [:label.form-label
        {:for "income-statement-depth"}
        "Depth"]
       (if @busy?
         [:p.placeholder-glow
          [:span.placeholder.col-12]]
         [:<>
          [:input#income-statement-depth.form-range
           {:type :range
            :min 1
            :max @max-depth
            :step 1
            :value (or (some-> @depth inc) @max-depth)
            :list "income-statement-depth-markers"
            :on-change (fn [e]
                         (let [v (dom/value (dom/target e))]
                           (swap! options assoc :depth (dec (parse-long v)))))}]
          [:div.d-flex.justify-content-between.px-1
           {:style {:margin-top "-10px"}}
           (for [x (range @max-depth)]
             ^{:key (str "income-statement-depth-" x)}
             [:span.border-start {:style {:height "10px"}}])]])])))

(defn- income-statement-options
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        options (r/cursor page-state [:income-statement :options])]
    (fn []
      (when (= :income-statement @selected)
        [:form {:no-validate true
                :on-submit (fn [e]
                             (.preventDefault e)
                             (v/validate options)
                             (when (v/valid? options)
                               (load-report page-state)))}
         [income-statement-option-fields page-state]]))))

(defn- income-statement-header
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :income-statement @selected))
        start-date (r/cursor page-state [:income-statement :options :start-date])
        end-date (r/cursor page-state [:income-statement :options :end-date])]
    (fn []
      [:span {:class (when @hide? "d-none")}
       (format-date @start-date)
       " - "
       (format-date @end-date)])))

(defn- income-statement
  [page-state]
  (let [options (r/cursor page-state [:income-statement :options])
        hide-zeros? (r/cursor options [:hide-zeros?])
        depth (r/cursor options [:depth])
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :income-statement @selected))
        report (r/cursor page-state [:income-statement :report])]
    (fn []
      [:div.d-flex
       {:class (when @hide? "d-none")}
       [:table.table.table-hover.table-borderless
        {:style {:max-width "40em"}}
        [:tbody
         (if @report
           (doall (map #(report-row % @hide-zeros?)
                       (apply-depth @depth @report)))
           [:tr
            [:td.text-center
             [bs/spinner]]])]]
       [:div.ms-auto.ps-3.border-start.d-print-none.d-none.d-md-inline
        [income-statement-options page-state]]])))

(defmethod load-report :balance-sheet
  [page-state]
  (+busy)
  (swap! page-state update-in [:balance-sheet] dissoc :report)
  (rpt/balance-sheet (get-in @page-state [:balance-sheet :options])
                     :callback -busy
                     :on-success (fn [report]
                                   (let [max-d (report-max-depth report)]
                                     (swap! page-state
                                            #(-> %
                                                 (assoc-in [:balance-sheet :report] report)
                                                 (assoc-in [:balance-sheet :options :depth] (dec max-d))))))))

(defn- balance-sheet-option-fields
  [page-state]
  (let [report (r/cursor page-state [:balance-sheet :report])
        max-depth (make-reaction #(report-max-depth @report))
        options (r/cursor page-state [:balance-sheet :options])
        depth (r/cursor options [:depth])]
    (fn []
      [:<>
       [forms/date-field
        options
        [:as-of]
        {:placeholder "As Of"
         :validate [:required]}]
       [:div.mt-3
        [:button.btn.btn-primary
         {:type :submit
          :data-bs-dismiss :offcanvas
          :title "Click here to show the report with the specified parameters"}
         (icon-with-text :arrow-repeat "Show")]]
       [:hr]
       [forms/checkbox-field
        options
        [:hide-zeros?]
        {:caption "Hide Zero-Balance Accounts"}]
       [:label.form-label
        {:for "balance-sheet-depth"}
        "Depth"]
       (if @busy?
         [:p.placeholder-glow
          [:span.placeholder.col-12]]
         [:<> [:input#balance-sheet-depth.form-range
               {:type :range
                :min 1
                :max @max-depth
                :step 1
                :value (or (some-> @depth inc) @max-depth)
                :list "balance-sheet-depth-markers"
                :on-change (fn [e]
                             (let [v (dom/value (dom/target e))]
                               (swap! options assoc :depth (dec (parse-long v)))))}]
          [:div.d-flex.justify-content-between.px-1
           {:style {:margin-top "-10px"}}
           (for [x (range @max-depth)]
             ^{:key (str "balance-sheet-depth-" x)}
             [:span.border-start {:style {:height "10px"}}])]])])))

(defn- balance-sheet-options
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        options (r/cursor page-state [:balance-sheet :options])]
    (fn []
      (when (= :balance-sheet @selected)
        [:form {:no-validate true
                :on-submit (fn [e]
                             (.preventDefault e)
                             (v/validate options)
                             (when (v/valid? options)
                               (load-report page-state)))}
         [balance-sheet-option-fields page-state]]))))

(defn- balance-sheet-header
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :balance-sheet @selected))
        as-of (r/cursor page-state [:balance-sheet :options :as-of])]
    (fn []
      [:span {:class (when @hide? "d-none")}
       (format-date @as-of)])))

(defn- balance-sheet
  [page-state]
  (let [options (r/cursor page-state [:balance-sheet :options])
        hide-zeros? (r/cursor options [:hide-zeros?])
        depth (r/cursor options [:depth])
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :balance-sheet @selected))
        report (r/cursor page-state [:balance-sheet :report])]
    (fn []
      [:div.d-flex
       {:class (when @hide? "d-none")}
       [:table.table.table-hover.table-borderless
        {:style {:max-width "40em"}}
        [:tbody
         (if @report
           (doall (map #(report-row % @hide-zeros?)
                       (apply-depth @depth @report)))
           [:tr
            [:td.text-center
             [bs/spinner]]])]]
       [:div.ms-auto.ps-3.border-start.d-print-none.d-none.d-md-inline
        [balance-sheet-options page-state]]])))

(defn- receive-budget-report
  [page-state]
  (fn [report]
    (let [max-d (budget-max-depth report)]
      (swap! page-state
             #(-> %
                  (assoc-in [:budget :report] report)
                  (assoc-in [:budget :options :depth] (dec max-d))
                  (update-in [:budget] dissoc :apply-info))))))

(defmethod load-report :budget
  [page-state]
  (+busy)
  (swap! page-state update-in [:budget] dissoc :report)
  (let [opts (get-in @page-state [:budget :options])]
    (if (:budget-id opts)
      (rpt/budget (dissoc opts :depth)
                  :callback -busy
                  :on-success (receive-budget-report page-state))
      (swap! page-state assoc-in [:budget :report] []))))

(defn- budget-options
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        options (r/cursor page-state [:budget :options])
        budgets (r/cursor page-state [:budgets])
        budget-items (make-reaction #(->> (vals @budgets)
                                          (sort-by :budget/start-date t/after?)
                                          (map (juxt :id :budget/name))))
        report (r/cursor page-state [:budget :report])
        depth (r/cursor options [:depth])
        max-depth (make-reaction #(budget-max-depth @report))]
    (fn []
      (when (and (= :budget @selected)
                 (seq @budget-items))
        [:<>
         [forms/select-field options [:budget-id] budget-items]
         [:label.form-label.mt-2
          {:for "budget-depth"}
          "Depth"]
         [:input#budget-depth.form-range
          {:type :range
           :min 1
           :max @max-depth
           :step 1
           :value (or (some-> @depth inc) @max-depth)
           :list "budget-depth-markers"
           :on-change (fn [e]
                        (let [v (dom/value (dom/target e))]
                          (swap! options assoc :depth (dec (parse-long v)))))}]
         [:div.d-flex.justify-content-between.px-1
          {:style {:margin-top "-10px"}}
          (for [x (range @max-depth)]
            ^{:key (str "budget-depth-" x)}
            [:span.border-start {:style {:height "10px"}}])]]))))

(defn- receive-budget
  [{account-id :id
    :report/keys [actual-per-period]
    :as report-item}
   page-state]
  (fn [budget]
    (let [budget (update-in budget
                            [:budget/items]
                            (partial index-by (comp :id :budget-item/account)))
          budget-item (or (get-in budget [:budget/items account-id])
                          #:budget-item{:account-id account-id
                                        :periods []})
          periods (vec
                    (repeat (get-in budget [:budget/period 0])
                            actual-per-period))]
      (swap! page-state
             assoc-in [:budget :apply-info] {:budget budget
                                             :budget-item (assoc budget-item :budget-item/periods periods)
                                             :report-item report-item}))))

(defn- apply-to-budget
  [report-item page-state]
  (+busy)
  (let [{{{:keys [budget-id]} :options} :budget} @page-state]
    (bdt/find budget-id
              :callback -busy
              :on-success (receive-budget report-item page-state))))

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
  (bdt/select {}
              :callback -busy
              :on-success (receive-budgets page-state)))

(defn- refine-items
  [depth items]
  (->> items
       (remove #(> (:report/depth %) depth))
       (remove #(and (< (:report/depth %) depth)
                     (:report/roll-up %)))
       (map #(if (= depth (:report/depth %))
               (merge % (:report/roll-up %))
               %))
       (remove #(= 0M (:report/actual %) (:report/budget %)))
       (sort-by :report/difference d/<)))

(defn- refine-and-flatten
  [depth groups]
  (mapcat #(concat [%]
                   (refine-items depth (:report/items %)))
          groups))

(defn- save-budget
  [page-state]
  (let [{:keys [budget budget-item]} (get-in @page-state [:budget :apply-info])]
    (+busy)
    (bdt/save (update-in budget
                         [:budget/items]
                         (fn [items]
                           (map (fn [item]
                                  (if (id= budget-item item)
                                    budget-item
                                    item))
                                (vals items))))
              :callback -busy
              :on-success #(load-report page-state))))

(defn- apply-budget-item-form
  [page-state]
  (let [apply-info (r/cursor page-state [:budget :apply-info])
        report-item (r/cursor apply-info [:report-item])
        account (r/cursor report-item [:account])
        budget (r/cursor apply-info [:budget])
        period-count (r/cursor budget [:budget/period 0])
        original-budget-item (make-reaction #(get-in @budget [:budget/items (:id @account)]))
        budget-item (r/cursor apply-info [:budget-item])
        original-total (make-reaction
                         #(decimal/sum (:budget-item/periods @original-budget-item)))
        item-total (make-reaction
                     #(decimal/sum (:budget-item/periods @budget-item)))]
    (fn []
      [:div.background.fixed-top {:class (when-not @apply-info "d-none")}
       [:div.card
        [:div.card-header (str "Apply Budget Item: " (string/join "/" (:account/path @account)))]
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
            (->> (range @period-count)
                 (map-indexed (fn [index _]
                                ^{:key (str "budget-item-period-" index)}
                                [:tr
                                 [:td (period-description index @budget)]
                                 [:td.text-end (format-decimal
                                                 (get-in @original-budget-item
                                                         [:budget-item/periods index]))]
                                 [:td [forms/decimal-input budget-item [:budget-item/periods index]]]]))
                 doall)]
           [:tfoot
            [:tr
             [:td.text-end {:col-span 2} (format-decimal @original-total)]
             [:td.text-end (format-decimal @item-total)]]]]]]
        [:div.card-footer
         [:button.btn.btn-primary {:title "Click here to save this budget item."
                                   :on-click #(save-budget page-state)}
          (icon-with-text :check "Save")]
         [:button.btn.btn-secondary.ms-2
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
         (cond
           (seq @report)
           (->> (:items @report)
                (refine-and-flatten @depth)
                (map (comp
                       #(budget-report-row % page-state)
                       #(assoc % :account (get-in @accounts-by-id [(:id %)]))))
                doall)

           @report
           [:tr
            [:td.text-center {:col-span 6}
             [:span.text-secondary-ephasis "No data."]]]

           :else
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
  [:tr {:class (cond-> [(str "report-" (name style))]
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

(defn- empty-position?
  [{:report/keys [style current-value shares-owned]}]
  (and (not= style :summary)
       (d/zero? (or current-value 0M))
       (d/zero? (or shares-owned 0M))))

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
          (->> @report
               (remove empty-position?)
               (map #(portfolio-report-row % @visible-ids page-state))
               doall)

          :else
          [:tr [:td.inline-status {:col-span 7} "No investment accounts found."]])]])))

(defn- portfolio-options
  [page-state]
  (let [options (r/cursor page-state [:portfolio :options])
        current-nav (r/cursor options [:filter :aggregate])]
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
       [forms/date-field options [:filter :as-of]]])))

(defn- portfolio-header
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :portfolio @selected))
        as-of (r/cursor page-state [:portfolio :options :filter :as-of])]
    (fn []
      [:span {:class (when @hide? "d-none")}
       (format-date @as-of)])))

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
  (let [selected (r/cursor page-state [:selected])
        options (r/cursor page-state [@selected :options])]
    [:form {:no-validate true
            :on-submit (fn [e]
                         (.preventDefault e)
                         (v/validate options)
                         (when (v/valid? options)
                           (load-report page-state)))}
     (case @selected
       :income-statement [income-statement-option-fields page-state]
       :balance-sheet    [balance-sheet-option-fields page-state]
       :budget           [budget-options page-state]
       :portfolio        [portfolio-options page-state])
     [:div.mt-3
      [:button.btn.btn-primary
       {:type :submit
        :data-bs-dismiss :offcanvas
        :title "Click here to show the report with the specified parameters"}
       (icon-with-text :arrow-repeat "Show")]]]))

(defn- filter-elem
  [page-state]
  (fn []
    [:div#report-options.offcanvas.offcanvas-end {:tab-index -1}
     [:div.offcanvas-header.d-flex.justify-content-between
      [:h3 "Options"]
      [:button.btn-close.text-reset {:data-bs-dismiss :offcanvas}]]
     [:div.offcanvas-body
      [filter-form page-state]]]))

(defn- init-state []
  (r/atom {:selected :balance-sheet
           :income-statement {:options
                              {:start-date (start-of-year)
                               :end-date (t/today)
                               :hide-zeros? true
                               :depth nil}}
           :balance-sheet {:options {:as-of (t/today)
                                     :hide-zeros? true
                                     :depth nil}}
           :budget {:options {:depth 0
                              :tags [:tax :mandatory :discretionary]}} ; TODO: make this user editable
           :portfolio {:options {:filter {:aggregate :by-account
                                          :as-of (t/today)}
                                 :by-account {:visible-ids #{}}
                                 :by-commodity {:visible-ids #{}}}}}))

(defn- small-nav
  [page-state]
  (fn []
    [:div.d-md-none.mt-2
     [forms/select-elem
      page-state
      [:selected]
      (map #(vector % (humanize %)) report-types)
      {:transform-fn keyword
       :on-change (fn [s field]
                    (when-not (get-in @page-state [(get-in @s field) :report])
                      (load-report page-state)))}]]))

(defn- big-nav
  [page-state]
  (fn []
    (bs/nav-tabs {:class "d-none d-md-flex"}
                 (map (report-nav-item-fn page-state)
                      report-types))))

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
        [:button.btn.btn-outline-secondary
         {:type :button
          :class (when (#{:balance-sheet :income-statement} @selected)
                   "d-md-none")
          :data-bs-toggle "offcanvas"
          :data-bs-target "#report-options"
          :aria-controls "report-options" }
         (icon :gear)]]

       [:div.d-none.d-print-block.text-center
        [:h1 (humanize @selected)]
        [:h2
         [income-statement-header page-state]
         [balance-sheet-header page-state]
         [budget-header page-state]
         [portfolio-header page-state]]]
       [:div.d-print-none
        [filter-elem page-state]
        [small-nav page-state]
        [big-nav page-state]]
       [:div.mt-3
        [income-statement page-state]
        [balance-sheet page-state]
        [budget page-state]
        [portfolio page-state]]])))

(secretary/defroute "/reports" []
  (swap! app-state assoc :page #'index :active-nav :reports))
