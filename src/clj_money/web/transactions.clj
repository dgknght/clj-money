(ns clj-money.web.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.schema :as schema])
  (:use [clj-money.web.shared :refer :all]))

(defn index
  ([entity-id] (index entity-id {}))
  ([entity-id options]
   (layout
     "Transactions" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th "Date"]
         [:th "Description"]]]
      [:a.btn.btn-primary {:href (str"/entities/" entity-id "/transactions/new")} "Add"]]])))

(defn new-transaction
  ([entity-id] (new-transaction entity-id {} {}))
  ([entity-id transaction options]
   (layout
     "New Transaction" options
     [:div.row
      [:div.col-md-6
       [:form {:action (str "/entities/" entity-id "/transactions") :method :post}
        (text-input-field transaction :transaction-date {:autofocus true
                                                         :class "date-field"})
        (text-input-field transaction :description)
        [:input.btn.btn-primary {:type :submit :value "Save"}]]]])))
