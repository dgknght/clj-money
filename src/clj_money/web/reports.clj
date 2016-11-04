(ns clj-money.web.reports
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-time.core :as t]
            [clj-money.util :refer [format-number
                                    parse-date]]
            [clj-money.web.shared :refer :all]
            [clj-money.models.reports :as reports]))

(defmulti render-report
  (fn [params]
    (:type params)))

(defn- report-row
  [record]
  [:tr {:class (case (:style record)
                 :header :report-header
                 :summary :report-summary
                 :data :report-data)}
   [:td
    [:span {:class (str "account-depth-" (:depth record))}
     (:caption record)]]
   [:td.text-right
    [:span {:class (str "balance-depth-" + (:depth record))}
     (format-number (:value record))]]])

(defmethod render-report :income-statement
  [{:keys [entity-id start-date end-date]}]
  [:table.table.table-striped
     (map report-row (reports/income-statement (env :db)
                                               entity-id
                                               (parse-date start-date)
                                               (parse-date end-date)))])

(defmethod render-report :balance-sheet
  [params]
  [:h1 "Balance Sheet"])

(defmulti render-filter
  (fn [params]
    (:type params)))

(defmethod render-filter :income-statement
  [params]
  (let [params (-> params
                   (update-in [:start-date] parse-date)
                   (update-in [:end-date] parse-date))]
    [:form {:action "#" :method :get}
     [:div.form-group
      [:label.control-label {:for :start-date} "Start Date"]
      [:input.form-control.date-field {:type :text
                                       :name :start-date
                                       :value (:start-date params)}]]
     [:div.form-group
      [:label {:for :end-date} "End Date"]
      [:input.form-control.date-field {:type :text
                                       :name :end-date
                                       :value (:end-date params)}]]
     [:input.btn.btn-primary {:type :submit :value "Show"}]]))

(defmethod render-filter :balance-sheet
  [params]
  )

(defn render
  [params]
  (let [params (-> params
                   (update-in [:entity-id] #(Integer. %))
                   (update-in [:type] keyword))]
    (layout
      "Reports" {}
      [:div.row
       [:div.col-md-12
        (tabbed-nav [{:id :income-statement
                      :caption "Income Statment"
                      :url (format "/entities/%s/reports/income-statement" (:entity-id params))}
                     {:id :balance-sheet
                      :caption "Balance Sheet"
                      :url (format "/entities/%s/reports/balance-sheet" (:entity-id params))}]
                    (:type params))]]
      [:div.row
       [:div.col-md-4.col-md-offset-1
        (render-filter params)]
       [:div.col-md-6
        (render-report params)]])))
