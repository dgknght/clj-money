(ns clj-money.web.reports
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-time.core :as t]
            [clj-money.web.shared :refer :all]
            [clj-money.models.reports :as reports]))

(defmulti render-report
  (fn [params]
    (:type params)))

(defmethod render-report :income-statement
  [{:keys [entity-id start end]}]
  [:pre (reports/income-statement (env :db) entity-id start end)])

(defmethod render-report :balance-sheet
  [params]
  [:h1 "Balance Sheet"])

(defn render
  [entity-id id]
  (layout
    "Reports" {}
    [:div.row
     [:div.col-md-12
      (tabbed-nav [{:id :income-statement
                    :caption "Income Statment"
                    :url (format "/entities/%s/reports/income-statement" entity-id)}
                   {:id :balance-sheet
                    :caption "Balance Sheet"
                    :url (format "/entities/%s/reports/balance-sheet" entity-id)}]
                  id)
      (render-report id entity-id)]]))
