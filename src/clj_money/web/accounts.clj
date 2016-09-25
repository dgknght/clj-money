(ns clj-money.web.accounts
  (:require [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.models.accounts :as accounts]
            [clj-money.schema :as schema])
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
       (let [accounts (accounts/select-by-entity-id (env :db) (Integer. entity-id))]
         (map account-row accounts))]
      [:a.btn.btn-primary
       {:href (format "/entities/%s/accounts/new" entity-id)
        :title "Click here to add a new account."}
       "Add"]]]))

(defn new-account
  "Renders the new account form"
  ([entity-id] (new-account entity-id {}))
  ([entity-id account]
   (layout
     "New account" {}
     [:div.row
      [:div.col-md-6
       [:form {:action (str "/entities/" entity-id "/accounts")
               :method :post}
        (text-input-field account :name {:autofocus true})
        (select-field account :type [{:value :asset     :caption "Asset"}
                                     {:value :liability :caption "Liability"}
                                     {:value :equity    :caption "Equity"}
                                     {:value :income    :caption "Income"}
                                     {:value :expense   :caption "Expense"}])
        [:input.btn.btn-primary {:type :submit
                                 :value "Save"
                                 :title "Click here to save the account"}]]]])))

(defn create
  "Creates the account and redirects to the index page on success, or
  re-renders the new form on failure"
  [params]
  (let [account (select-keys params [:entity-id :name :type])]
    (try
      (accounts/create (env :db) params)
      (redirect (str "/entities/" (:entity-id params) "/accounts"))
      (catch clojure.lang.ExceptionInfo e
        (new-account (schema/append-errors params (ex-data e)))))))
