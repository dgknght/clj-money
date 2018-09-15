(ns clj-money.views.transactions
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.transactions :as transactions]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.util :as util]
            [clj-money.forms :refer [text-input
                                     select-input
                                     typeahead-input
                                     required]]))

(defn- transactions-page
  []
  (let [transactions (r/atom [])]
    (transactions/search {}
                         #(reset! transactions %)
                         notify/danger)
    (with-layout
      [:section
       [:h1 "Transactions" ]
       "Transactions will go here"])))

(secretary/defroute accounts-path "/transactions" []
  (r/render [transactions-page] (app-element)))
