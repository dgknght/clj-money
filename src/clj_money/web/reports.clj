(ns clj-money.web.reports
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-money.web.shared :refer :all]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(defmulti render-report
  (fn [type entity-id]
    type))

(defmethod render-report
  :income-statement
  [_ entity-id]
  [:h1 "Income Statement"])

(defmethod render-report
  :balance-sheet
  [_ entity-id]
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
