(ns clj-money.web.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.models.accounts :as accounts]
            [clj-money.schema :as schema])
  (:use [clj-money.web.shared :refer :all]))

(defn- account-rows
  [[type accounts]]
  (html
    [:tr
     [:td.account-type {:colspan 2} type]]
    (map #(vector :tr
                  [:td (:name %)]
                  [:td
                   [:span.btn-group
                    (glyph-button :pencil
                                  (format "/accounts/%s/edit" (:id %))
                                  {:level :info
                                   :size :extra-small})
                    (glyph-button :remove
                                  (format "/accounts/%s/delete" (:id %))
                                  {:level :danger
                                   :size :extra-small
                                   :data-method :post
                                   :data-confirm "Are you sure you want to delete this account?"})]])
         accounts)))

(defn index
  "Renders the list of accounts"
  ([entity-id] (index entity-id {}))
  ([entity-id options]
   (layout
     "Accounts" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th.col-md-10 "Name"]
         [:th.col-sm-2 "&nbsp;"]]
        (let [groups (accounts/group-by-type (env :db) (Integer. entity-id))]
          (map account-rows groups))]
       [:a.btn.btn-primary
        {:href (format "/entities/%s/accounts/new" entity-id)
         :title "Click here to add a new account."}
        "Add"]]])))

(defn- form-fields
  "Renders the form fields for an account"
  [account]
  (html
    (text-input-field account :name {:autofocus true})
    (select-field account :type [{:value :asset     :caption "Asset"}
                                 {:value :liability :caption "Liability"}
                                 {:value :equity    :caption "Equity"}
                                 {:value :income    :caption "Income"}
                                 {:value :expense   :caption "Expense"}])
    [:input.btn.btn-primary {:type :submit
                             :value "Save"
                             :title "Click here to save the account"}]
    [:a.btn.btn-default {:href (format "/entities/%s/accounts" (:entity-id account))
                         :title "Click here to return to the list of accounts."}
     "Back"]))

(defn new-account
  "Renders the new account form"
  ([entity-id] (new-account entity-id {:entity-id entity-id}))
  ([entity-id account]
   (layout
     "New account" {}
     [:div.row
      [:div.col-md-6
       [:form {:action (str "/entities/" entity-id "/accounts")
               :method :post}
        (form-fields account)]]])))

(defn create
  "Creates the account and redirects to the index page on success, or
  re-renders the new form on failure"
  [params]
  (let [account (select-keys params [:entity-id :name :type])]
    (try
      (accounts/create (env :db) params)
      (redirect (str "/entities/" (:entity-id params) "/accounts"))
      (catch clojure.lang.ExceptionInfo e
        (new-account (:entity-id params) (schema/append-errors params (ex-data e)))))))

(defn edit
  "Renders the edit form for an account"
  [id-or-account]
  (layout
    "Edit account" {}
    (let [account (if (map? id-or-account)
                    id-or-account
                    (accounts/find-by-id (env :db) (Integer. id-or-account)))]
      [:div.row
       [:div.col-md-6
        [:form {:action (format "/accounts/%s" (:id account))
                :method :post}
         (form-fields account)]]])))

(defn update
  "Updates the account and redirects to the account list on
  success or rerenders the edit from on error"
  [params]
  (try
    (let [updated (accounts/update (env :db)
                                   (select-keys params [:id :name :type]))]
      (redirect (format "/entities/%s/accounts" (:entity-id updated))))
    (catch clojure.lang.ExceptionInfo e

      (log/debug "Unable to update account " params ": " (ex-data e))

      (edit (schema/append-errors params (ex-data e))))))

(defn delete
  "Deletes the specified account"
  [id]
  (let [account (accounts/find-by-id (env :db) (Integer. id))]
    (try
      (accounts/delete (env :db) (:id account))
      (redirect (format "/entities/%s/accounts" (:entity-id account)))
      (catch Exception e
        (log/error e "Unable to delete account id=" id)
        (index (:entity-id account) {:alerts [{:type :danger
                                               :message (html [:strong "Unable to delete the account."]
                                                              "&nbsp;"
                                                              (.getMessage e))}]})))))
