(ns clj-money.web.accounts
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-money.models.accounts :as accounts])
  (:use [clj-money.web.shared :refer :all]))

(defn- account-row
  [account]
  [:tr
   [:td (:name account)]
   [:td (:type account)]])

(defn index
  "Renders the list of accounts"
  [entity-id]
  (layout
    "Accounts" {}
    [:div.row
     [:div.col-md-6
      [:table.table.table-striped
       [:tr
        [:th "Name"]
        [:th "Type"]]
       (let [accounts (accounts/select-by-entity-id (env :db) entity-id)]
         (map account-row accounts))]
      [:a.btn.btn-primary {:href (format "/entities/%s/accounts/new" entity-id)}]]]))
