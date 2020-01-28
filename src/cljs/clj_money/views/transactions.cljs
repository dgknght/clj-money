(ns clj-money.views.transactions
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [clj-money.api.transactions :as transactions]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]))

(defn- transactions-page
  []
  (let [transactions (r/atom [])]
    (transactions/search {}
                         #(reset! transactions %)
                         notify/danger)
    [:section
       [:h1 "Transactions" ]
       "Transactions will go here"]))

(secretary/defroute accounts-path "/transactions" []
  (r/render [transactions-page] (app-element)))
