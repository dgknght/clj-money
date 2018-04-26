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
                                    parse-local-date]]
            [clj-money.web.shared :refer :all]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             tag-resource]]
            [clj-money.permissions.reports]
            [clj-money.models.budgets :as budgets]
            [clj-money.reports :as reports]))

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

(defn- default-start-date
  []
  (let [today (t/today)]
    (t/local-date (t/year today)
                  (t/month today)
                  1)))

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

(defn- budget-report-row-class
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
  (if-let [budget (if budget-id
                    (budgets/find-by-id (env :db) budget-id)
                    (budgets/find-by-date (env :db) entity-id as-of))]
    (let [as-of (or as-of
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
                                                        as-of)))]))
    (html
        [:h2 "No budget found"]
        [:p "No budget was found for the specified time period"])))

(defmulti render-filter
  (fn [params]
    (:type params)))

(defmethod render-filter :income-statement
  [params]
  [:form {:action "#" :method :get}
   (date-input-field params :start-date)
   (date-input-field params :end-date)
   [:input.btn.btn-primary {:type :submit :value "Show"}]])

(defmethod render-filter :balance-sheet
  [params]
  [:form {:action "#" :method :get}
   (date-input-field params :as-of)
   [:input.btn.btn-primary {:type :submit :value "Show"}]])

(defmethod render-filter :budget
  [params]
  [:form {:action "#" :method :get}
   [:div.form-group
    [:label.control-label {:for :budget-id} "Budget"]
    [:select.form-control {:name "budget-id"}
     (map #(vector :option {:value (:id %)} (:name %)) (budgets/search (env :db) {:entity-id (:entity-id params)}))] ]
   (date-input-field params :as-of)
   [:input.btn.btn-primary {:type :submit :value "Show"}]])

(defn- report-nav
  [entity report-spec]
  (let [nav-items (->> [{:type :income-statement
                         :caption "Income Statement"}
                        {:type :balance-sheet
                         :caption "Balance Sheet"}
                        {:type :budget
                         :caption "Budget"}]
                       (map #(assoc % :entity-id (:id entity)))
                       (map #(tag-resource % :report))
                       (filter #(allowed? (:type %) %))
                       (map #(-> %
                                 (assoc :id (:type %)
                                        :url (format "/entities/%s/reports/%s"
                                                     (:id entity)
                                                     (-> % :type name))))))]
    (tabbed-nav nav-items (:type report-spec))))

(defn render
  [{{entity :entity :as params} :params}]
  (let [report-spec (-> params ; TODO separate default based on the report type
                        (select-keys [:entity-id :type :start-date :end-date :as-of])
                        (update-in [:type] (fnil keyword :balance-sheet))
                        (assoc :start-date (or (parse-local-date (:start-date params))
                                               (default-start-date))
                               :end-date (or (parse-local-date (:end-date params))
                                             (default-end-date))
                               :as-of (or (parse-local-date (:as-of params))
                                          (default-end-date)))
                        (tag-resource :report)
                        (authorize (:type params)))]
    (with-layout "Reports" {:entity entity}
      [:div.row
       [:div.col-md-12
        (report-nav entity report-spec)]]
      ; This layout should change based on report
      [:div.row
       [:div#report-settings.col-md-2
        (render-filter report-spec)]
       [:div.col-md-10
        (render-report report-spec)]])))
