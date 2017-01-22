(ns clj-money.web.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.url :refer :all]
            [clj-money.inflection :refer [humanize]]
            [clj-money.util :refer [format-number]]
            [clj-money.pagination :as pagination]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.web.money-shared :refer [grouped-options-for-accounts
                                                budget-monitors]])
  (:use [clj-money.web.shared :refer :all]))

(defmacro with-accounts-layout
  [page-title entity-id options & content]
  `(with-layout
     ~page-title (assoc ~options :side-bar (budget-monitors (Integer. ~entity-id)))
     ~@content))

(defn- account-row
  "Renders a single account row"
  [account depth]
  [:tr
   [:td
    [:span {:class (format "account-depth-%s" depth)}
     (:name account)]]
   [:td.text-right
    [:span {:class (format "balance-depth-%s" depth)}
     (format-number (+ (:balance account) (:children-balance account)))]]
   [:td
    [:span.btn-group
     (glyph-button :pencil
                   (format "/accounts/%s/edit" (:id account))
                   {:level :info
                    :size :extra-small
                    :title "Click here to edit this account"})
     (glyph-button :list-alt
                   (format "/accounts/%s" (:id account))
                   {:level :default
                    :size :extra-small
                    :title "Click here to view transactions for this account"})
     (glyph-button :check
                   (format "/accounts/%s/reconciliations/new" (:id account))
                   {:level :default
                    :size :extra-small
                    :title "Click here to reconcile this account"})
     (glyph-button :remove
                   (format "/accounts/%s/delete" (:id account))
                   {:level :danger
                    :size :extra-small
                    :data-method :post
                    :data-confirm "Are you sure you want to delete this account?"
                    :title "Click here to remove this account"})]]])

(defn- account-and-children-rows
  "Renders an individual account row and any child rows"
  ([account] (account-and-children-rows account 0))
  ([account depth]
   (html
     (concat
       [(account-row account depth)]
       (->> (:children account)
            (map #(account-and-children-rows % (+ depth 1)))
            (into []))))))

(defn- account-rows
  "Renders rows for all accounts and type headers"
  [{:keys [type accounts]}]
  (html
    [:tr.account-type
     [:td type]
     [:td.text-right (->> accounts
                          (map (juxt :balance :children-balance))
                          (reduce (fn [sum [balance children-balance]]
                                    (+ sum balance children-balance))
                                  0)
                          format-number)]
     [:td "&nbsp;"]]
    (map account-and-children-rows accounts)))

(defn index
  "Renders the list of accounts"
  ([req] (index req {}))
  ([{params :params} options]
   (let [entity-id (Integer. (:entity-id params))]
     (with-accounts-layout "Accounts" entity-id options
       [:table.table.table-striped
        [:tr
         [:th.col-sm-6 "Name"]
         [:th.col-sm-4.text-right "Balance"]
         [:th.col-sm-2 "&nbsp;"]]
        (let [groups (accounts/select-nested-by-entity-id (env :db) (Integer. entity-id))]
          (map account-rows groups))]
       [:a.btn.btn-primary
        {:href (format "/entities/%s/accounts/new" entity-id)
         :title "Click here to add a new account."}
        "Add"]))))

(defn- transaction-item-row
  [{:keys [transaction-id
           transaction-date
           description
           polarized-amount
           reconciled?
           account-id
           balance] :as item}]
  [:tr
   [:td.text-right transaction-date]
   [:td description]
   [:td.text-right polarized-amount]
   [:td.text-right balance]
   [:td.text-center [:span.glyphicon
                     {:aria-hidden "true"
                      :class (if reconciled? "glyphicon-check" "glyphicon-unchecked")}]]
   [:td
    [:span.btn-group
     (glyph-button :pencil
                   (-> (path "/transactions" transaction-id "edit")
                       (query {:redirect (url-encode (format "/accounts/%s" account-id))})
                       format-url)
                   {:level :info
                    :size :extra-small
                    :title "Click here to edit this transaction."})
     (glyph-button :remove
                   (-> (path "/transactions" transaction-id "delete")
                       (query {:redirect (url-encode (format "/accounts/%s" account-id))})
                       format-url)
                   {:level :danger
                    :size :extra-small
                    :title "Click here to remove this transaction."
                    :data-method :post
                    :data-confirm "Are you sure you want to remove this transaction?"
                    :method :post})]]])

(defn show
  "Renders account details, including transactions"
  ([req] (show req {}))
  ([{params :params} options]
   (let [id (Integer. (:id params))
         account (accounts/find-by-id (env :db) id)]
     (with-accounts-layout (format "Account - %s" (:name account)) (:entity-id account) options
       [:table.table.table-striped.table-hover
        [:tr
         [:th.text-right "Date"]
         [:th "Description"]
         [:th.text-right "Amount"]
         [:th.text-right "Balance"]
         [:th.text-center "Rec."]
         [:th "&nbsp;"]]
        (map transaction-item-row
             (transactions/items-by-account (env :db)
                                            id
                                            (pagination/prepare-options params)))]
       (pagination/nav (assoc params
                              :url (-> (path "/accounts"
                                             (:id account)))
                              :total (transactions/count-items-by-account (env :db) (:id account))))
       [:a.btn.btn-primary
        {:href (-> (path "/entities"
                         (:entity-id account)
                         "transactions"
                         "new")
                   (query {:redirect (url-encode (format "/accounts/%s" id))})
                   format-url)
         :title "Click here to add a new transaction."}
        "Add"]
       "&nbsp;"
       [:a.btn.btn-default
        {:href (format "/accounts/%s/reconciliations/new" (:id account))
         :title "Click here to reconcile this account."}
        "Reconcile"]
       "&nbsp;"
       [:a.btn.btn-default
        {:href (format "/entities/%s/accounts" (:entity-id account))
         :title "Click here to return to the list of accounts."}
        "Back"]))))

(defn- form-fields
  "Renders the form fields for an account"
  [account]
  (html
    (text-input-field account :name {:autofocus true})
    (select-field account :type (map #(vector :option {:value %} (humanize %))
                                     accounts/account-types))
    (select-field account
                  :parent-id
                  (grouped-options-for-accounts (:entity-id account)
                                                {:include-none? true
                                                 :selected-id (:parent-id account)}))
    [:input.btn.btn-primary {:type :submit
                             :value "Save"
                             :title "Click here to save the account"}]
    "&nbsp;"
    [:a.btn.btn-default {:href (format "/entities/%s/accounts" (:entity-id account))
                         :title "Click here to return to the list of accounts."}
     "Back"]))

(defn new-account
  "Renders the new account form"
  ([req] (new-account req {:entity-id (Integer. (-> req :params :entity-id))}))
  ([{params :params} account]
   (let [entity-id (Integer. (:entity-id params))]
     (with-accounts-layout "New account" entity-id {}
       [:form {:action (str "/entities/" entity-id "/accounts")
               :method :post}
        (form-fields account)]))))

(defn create
  "Creates the account and redirects to the index page on success, or
  re-renders the new form on failure"
  [{params :params}]
  (let [account (select-keys params [:entity-id :name :type :parent-id])
        saved (accounts/create (env :db) account)]
    (if (validation/has-error? saved)
      (new-account {:params (select-keys saved [:entity-id])} saved)
      (redirect (str "/entities/" (:entity-id params) "/accounts")))))

(defn edit
  "Renders the edit form for an account"
  [req]
  (let [account (or (:account req)
                    (accounts/find-by-id (env :db) (Integer. (-> req :params :id))))]
    (with-accounts-layout "Edit account" (:entity-id account) {}
      [:form {:action (format "/accounts/%s" (:id account))
              :method :post}
       (form-fields account)])))

(defn update
  "Updates the account and redirects to the account list on
  success or rerenders the edit from on error"
  [{params :params}]
  (let [account (select-keys params [:id
                                     :name
                                     :type
                                     :parent-id])
        updated (accounts/update (env :db) account)]
    (if (validation/has-error? updated)
      (edit {:account updated})
      (redirect (format "/entities/%s/accounts" (:entity-id updated))))))

(defn delete
  "Deletes the specified account"
  [{{id :id} :params}]
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
