(ns clj-money.views.reports
  (:require [clojure.string :as string]
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
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts-by-id
                                     +busy
                                     -busy
                                     busy?]]
            [clj-money.budgets :refer [period-description]]
            [clj-money.api.budgets :as bdt]
            [clj-money.api.reports :as rpt]))

(defmulti ^:private report-row :style)

(defmethod ^:private report-row :header
  [{:keys [caption value]} _]
  ^{:key (str "report-row-" caption)}
  [:tr.report-header
   [:th {:scope :row} caption]
   [:td.text-end (format-decimal value)]])

(defmethod ^:private report-row :data
  [{:keys [id caption value depth]} hide-zeros?]
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
  [{:keys [caption value]} _]
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
  (rpt/income-statement (select-keys (:income-statement @page-state) [:start-date :end-date])
                        (map
                          (fn [result]
                            (-busy)
                            (swap! page-state assoc-in [:income-statement :report] result)))))

(defn- income-statement-options
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :income-statement @selected))]
    (fn []
      [:div {:class (when @hide? "d-none")}
       [forms/date-field
        page-state
        [:income-statement :start-date]
        {:placeholder "Start date"
         :validate [:required]}]
       [forms/date-field
        page-state
        [:income-statement :end-date]
        {:placeholder "End date"
         :validate [:required]}]
       [forms/checkbox-field
          page-state
          [:hide-zeros?]
          {:caption "Hide Zero-Balance Accounts"}]])))

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
  (let [hide-zeros? (r/cursor page-state [:hide-zeros?])
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
  (rpt/balance-sheet (select-keys (:balance-sheet @page-state) [:as-of])
                     (map (fn [result]
                            (-busy)
                            (swap! page-state assoc-in [:balance-sheet :report] result)))))

(defn- balance-sheet-options
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :balance-sheet @selected))]
    (fn []
      [:form {:class (when @hide? "d-none")
              :on-submit (fn [e]
                           (.preventDefault e)
                           false)
              :style {:max-width "512px"}}
       [forms/date-field
        page-state
        [:balance-sheet :as-of]
        {:placeholder "As Of"
         :validate [:required]}]
       [forms/checkbox-field
        page-state
        [:hide-zeros?]
        {:caption "Hide Zero-Balance Accounts"}]])))

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
  (let [hide-zeros? (r/cursor page-state [:hide-zeros?])
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

(defmethod load-report :budget
  [page-state]
  (+busy)
  (swap! page-state update-in [:budget] dissoc :report)
  (rpt/budget (select-keys (:budget @page-state) [:budget-id :tags])
              (map (fn [result]
                     (-busy)
                     (swap! page-state #(-> %
                                            (assoc-in [:budget :report] result)
                                            (update-in [:budget] dissoc :apply-info)))))))

(defn- budget-options
  [page-state]
  (let [budgets (r/cursor page-state [:budget :budgets])
        options (make-reaction #(->> (vals @budgets)
                                     (sort-by :start-date t/after?)
                                     (map (juxt :id :name))))
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :budget @selected))]
    (fn []
      [:div {:class (when @hide? "d-none")}
       [forms/select-field page-state [:budget :budget-id] options]
       [forms/integer-field page-state [:budget :depth] {:class "ms-sm-2"
                                                         :placeholder "Depth"
                                                         :style {:width "5em"}}]])))

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
              (map #(receive-budget % report-item page-state)))))

(defn- budget-report-row
  [{:keys [id
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
       (bs/icon :gear {:size :small})])
    [:span.flex-fill.text-end (format-decimal difference)]]
   [:td.text-end.d-none.d-md-table-cell (format-percent percent-difference)]
   [:td.text-end.d-none.d-md-table-cell (format-decimal actual-per-period)]])

(defn- load-budgets
  [page-state]
  (+busy)
  (bdt/search (map (fn [budgets]
                     (swap! page-state
                            (fn [state]
                              (-> state
                                  (assoc-in [:budget :budgets] (->> budgets
                                                                    (map (juxt :id identity))
                                                                    (into {})))
                                  (assoc-in [:budget :budget-id] (-> budgets first :id)))))))))

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
              (map (fn [& _]
                     (-busy)
                     (load-report page-state))))))

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
                 (map-indexed (fn [index]
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
         [bs/busy-button {:html {:class "btn-primary"
                                 :title "Click here to save this budget item."
                                 :on-click #(save-budget page-state)}
                          :icon :check
                          :caption "Save"
                          :busy? busy?}]
         [bs/busy-button {:html {:class "btn-light ms-2"
                                 :title "Click here to cancel this edit."
                                 :on-click #(swap! page-state update-in [:budget] dissoc :apply-info)}
                          :icon :x
                          :caption "Cancel"
                          :busy? busy?}]]]])))

(defn- budget
  [page-state]
  (let [report (r/cursor page-state [:budget :report])
        depth (r/cursor page-state [:budget :depth])
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
  (-busy)
  (let [{:keys [current-nav] :as state} (get-in @page-state [:portfolio])]
    (swap! page-state update-in [:portfolio current-nav] dissoc :report)
    (rpt/portfolio {:aggregate current-nav
                    :as-of (get-in state [:filter :as-of])}
                   (map (fn [result]
                          (swap! page-state
                                 assoc-in
                                 [:portfolio
                                  current-nav
                                  :report]
                                 result))))))

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
                  "text-end")}
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
  (let [current-nav (r/cursor page-state [:portfolio :current-nav])
        report (make-reaction #(get-in @page-state [:portfolio @current-nav :report]))
        visible-ids (make-reaction #(get-in @page-state [:portfolio @current-nav :visible-ids]))]
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
  [page-state]
  (let [current-nav (r/cursor page-state [:portfolio :current-nav])
        report-filter (r/cursor page-state [:portfolio :filter])
        selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :portfolio @selected))]
    (fn []
      [:div {:class (when @hide? "d-none")}
       (bs/nav-pills {:class "mb-2"} (map (fn [id]
                                            {:elem-key id
                                             :caption (humanize id)
                                             :on-click (fn []
                                                         (reset! current-nav id)
                                                         (load-report page-state))
                                             :active? (= id @current-nav)})
                                          [:by-account :by-commodity]))
       [forms/date-field report-filter [:as-of]]])))

(defn- portfolio
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        hide? (make-reaction #(not= :portfolio @selected))]
    (fn []
      [:div.mt-2 {:class (when @hide? "d-none")}
       [render-portfolio page-state]])))

(def ^:private report-types
  [:income-statement
   :balance-sheet
   :budget
   :portfolio])

(defn- report-nav-item-fn
  [page-state]
  (fn [id]
    (let [selected (get-in @page-state [:selected])]
      {:elem-key id
       :caption (title-case (humanize id))
       :active? (= id selected)
       :on-click (fn []
                   (swap! page-state assoc :selected id)
                   (when-not (get-in @page-state [id :report])
                     (load-report page-state)))})))

(defn- index []
  (let [page-state (r/atom {:selected :income-statement
                            :hide-zeros? true
                            :income-statement {:start-date (start-of-year)
                                               :end-date (t/today)}
                            :balance-sheet {:as-of (t/today)}
                            :budget {:depth 1
                                     :tags [:tax :mandatory :discretionary]} ; TODO: make this user editable
                            :portfolio {:current-nav :by-account
                                        :filter {:as-of (t/today)}
                                        :by-account {:visible-ids #{}}
                                        :by-commodity {:visible-ids #{}}}})
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
        [:button.btn.btn-light
         {:type :button
          :data-bs-toggle "offcanvas"
          :data-bs-target "#report-options"
          :aria-controls "report-options" }
         (bs/icon :gear)]]

       [:div.d-none.d-print-block.text-center
        [:h1 (humanize @selected)]
        [:h2
         [income-statement-header page-state]
         [balance-sheet-header page-state] ]]
       [:div.d-print-none
        [:div.d-md-none.mt-2
         [forms/select-elem
          page-state
          [:selected]
          (map #(vector % (humanize %)) report-types)
          {:transform-fn keyword
           :on-change (fn [s field]
                        (when-not (get-in @page-state [(get-in @s field) :report])
                         (load-report page-state)))}]]
        (bs/nav-tabs {:class "d-none d-md-flex"}
                     (map (report-nav-item-fn page-state)
                          report-types))]
       [:div#report-options.offcanvas.offcanvas-end {:tab-index -1}
        [:div.offcanvas-header.d-flex.justify-content-between
         [:h3 "Options"]
         [:button.btn-close.text-reset {:data-bs-dismiss :offcanvas}]]
        [:div.offcanvas-body
         [income-statement-options page-state]
         [balance-sheet-options page-state]
         [budget-options page-state]
         [portfolio-options page-state]
         [:div.mt-3
          [bs/busy-button {:html {:class "btn-primary"
                                  :on-click #(load-report page-state)
                                  :data-bs-dismiss :offcanvas
                                  :title "Click here to show the report with the specified parameters"}
                           :caption "Show"
                           :icon :arrow-repeat
                           :busy? busy?}]]]]
       [:div.mt-3
        [income-statement page-state]
        [balance-sheet page-state]
        [budget page-state]
        [portfolio page-state]]])))

(secretary/defroute "/reports" []
  (swap! app-state assoc :page #'index))
