(ns clj-money.web.reports
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-time.core :as t]
            [clj-money.util :refer [format-number
                                    format-date
                                    parse-date]]
            [clj-money.web.shared :refer :all]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.reports :as reports]))

(defmulti render-report
  (fn [params]
    (:type params)))

(defn- report-row
  [record]
  [:tr {:class (case (:style record)
                 :header :report-header
                 :summary :report-summary
                 :data :report-data
                 nil)}
   [:td
    [:span {:class (str "account-depth-" (:depth record))}
     (:caption record)]]
   [:td.text-right
    [:span {:class (str "balance-depth-" (:depth record))}
     (format-number (:value record))]]])

(defn- default-end-date
  []
  (let [today (t/today)]
    (t/local-date (t/year today)
                  (t/month today)
                  (t/number-of-days-in-the-month today))))

(defmethod render-report :income-statement
  [{:keys [entity-id start-date end-date]}]
  (html
    [:h2 (format "Income Statement: %s to %s"
                 (format-date start-date)
                 (format-date end-date))]
    [:table.table.table-striped
     (map report-row (reports/income-statement (env :db)
                                               entity-id
                                               start-date
                                               end-date))]))

(defmethod render-report :balance-sheet
  [{:keys [entity-id as-of]}]
  (html
    [:h2 (format "Balance Sheet as of %s" (format-date as-of))]
    [:table.table.table-striped
     (map report-row (reports/balance-sheet (env :db)
                                            entity-id
                                            as-of))]))

(defn-  budget-report-row-class
  [record]
  (string/join " " (cond-> []
                     true (conj (format "report-%s" (-> record :style name)))
                     (> 0M (:difference record)) (conj "negative"))))

(defn- budget-report-row
  [record]
  [:tr {:class (budget-report-row-class record)}
   [:td (:caption record)]
   [:td.text-right (-> record :budget format-number)]
   [:td.text-right (-> record :actual format-number)]
   [:td.text-right (-> record :difference format-number)]
   [:td.text-right (-> record :percent-difference format-number)]
   [:td.text-right (-> record :actual-per-period format-number)]])

(defmethod render-report :budget
  [{:keys [entity-id budget-id as-of] :as params}]
  (let [budget (if budget-id
                 (budgets/find-by-id (env :db) (Integer. budget-id))
                 (first (budgets/select-by-entity-id (env :db) entity-id))) ; TODO find the current budget
        as-of (or as-of
                  (budgets/end-date budget))]
    (html
      [:h2 (format "Budget %s as of %s" (:name budget) (format-date as-of))]
      [:table.table
       [:tr
        [:th "Account"]
        [:th.text-right "Budget"]
        [:th.text-right "Actual"]
        [:th.text-right "Diff."]
        [:th.text-right "% Diff."]
        [:th.text-right "Act./Period"]]
       (map budget-report-row (:items (reports/budget (env :db)
                                                      budget
                                                      as-of)))])))

(defmulti render-filter
  (fn [params]
    (:type params)))

(defmethod render-filter :income-statement
  [params]
  [:form {:action "#" :method :get}
   [:div.form-group
    [:label.control-label {:for :start-date} "Start Date"]
    [:input.form-control.date-field {:type :text
                                     :name :start-date
                                     :value (format-date (:start-date params))}]]
   [:div.form-group
    [:label {:for :end-date} "End Date"]
    [:input.form-control.date-field {:type :text
                                     :name :end-date
                                     :value (format-date (:end-date params))}]]
   [:input.btn.btn-primary {:type :submit :value "Show"}]])

(defmethod render-filter :balance-sheet
  [params]
  [:form {:action "#" :method :get}
   [:div.form-group
    [:label.control-label {:for :start-date} "As of"]
    [:input.form-control.date-field {:type :text
                                     :name :as-of
                                     :value (format-date (:as-of params))}]]
   [:input.btn.btn-primary {:type :submit :value "Show"}]])

(defmethod render-filter :budget
  [params]
  [:form {:action "#" :method :get}
   [:div.form-group
    [:label.control-label {:for :budget-id} "Budget"]
    [:select.form-control {:name "budget-id"}
     (map #(vector :option {:value (:id %)} (:name %)) (budgets/select-by-entity-id (env :db) (Integer. (:entity-id params))))] ]
   [:div.form-group
    [:label.control-label {:for :as-of} "As of"]
    [:input.form-control.date-field {:type :text
                                     :name :as-of
                                     :value (format-date (:as-of params))}]]
   [:input.btn.btn-primary {:type :submit :value "Show"}]])

(defn render
  [params]
  (let [params (-> params ; TODO separate default based on the report type
                   (update-in [:entity-id] #(Integer. %))
                   (update-in [:type] keyword)
                   (assoc :start-date (or (parse-date (:start-date params))
                                          (budgets/default-start-date))
                          :end-date (or (parse-date (:end-date params))
                                        (default-end-date))
                          :as-of (or (parse-date (:as-of params))
                                     (default-end-date))))]
    (with-layout "Reports" {}
      [:div.row
       [:div.col-md-12
        (tabbed-nav [{:id :income-statement
                      :caption "Income Statment"
                      :url (format "/entities/%s/reports/income-statement" (:entity-id params))}
                     {:id :balance-sheet
                      :caption "Balance Sheet"
                      :url (format "/entities/%s/reports/balance-sheet" (:entity-id params))}
                     {:id :budget
                      :caption "Budget"
                      :url (format "/entities/%s/reports/budget" (:entity-id params))}]
                    (:type params))]]

      ; This layout should change based on report
      [:div.row
       [:div.col-md-2
        (render-filter params)]
       [:div.col-md-10
        (render-report params)]])))
