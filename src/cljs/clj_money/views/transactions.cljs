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
  [account-id]
  (let [account (r/atom {})
        transactions (r/atom [])]
    (accounts/get-one account-id
                      #(reset! account %)
                      notify/danger)
    #_(transactions/search {:account-id account-id}
                         #(reset! transactions %)
                         notify/danger)
    (with-layout
      [:section
       [:h1 "Transactions for " (:name @account)]
       [:pre (prn-str @account)]
       "Transactions will go here"])))

(secretary/defroute accounts-path "/accounts/:account-id/transactions" [account-id]
  (r/render [transactions-page account-id] (app-element)))
