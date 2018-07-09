(ns clj-money.views.accounts
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.api.accounts :as accounts]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.util :as util]
            [clj-money.forms :refer [text-input
                                     select-input
                                     required]]))

(defn- delete
  [account accounts]
  (js/alert "not implemented."))

(defn- account-row
  [account accounts]
  ^{:key (str "account-" (:id account))}
  [:tr
   [:td (:name account)]
   [:td
    [:div.btn-group
     (util/link-to nil
                   (util/path :accounts (:id account) :edit)
                   {:icon :pencil
                    :class "btn btn-info btn-xs"
                    :title "Click here to edit this account."})
     (util/button nil
                  #(delete account accounts)
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this account."})]]])

(defn- account-list
  [accounts]
  [:table.table.table-striped.table-hover
   [:tbody
    [:tr
     [:th "Name"]
     [:th (util/space)]]
    (for [account @accounts]
      (account-row account accounts))]])

(defn- accounts-page []
  (with-layout
    [:section
     [:h1 "Accounts"]
     (let [accounts (r/atom [])]
       (accounts/get-all (:id @state/current-entity)
                         #(reset! accounts %)
                         notify/danger)
       [account-list accounts])
     (util/add-button "/accounts/new")]))

(secretary/defroute accounts-path "/accounts" []
  (r/render [accounts-page] (app-element)))
