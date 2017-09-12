(ns clj-money.web.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.authorization :refer [authorize]]
            [clj-money.url :refer :all]
            [clj-money.inflection :refer [humanize]]
            [clj-money.util :refer [format-number]]
            [clj-money.pagination :as pagination]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.lots :as lots]
            [clj-money.models.prices :as prices]
            [clj-money.web.money-shared :refer [grouped-options-for-accounts
                                                budget-monitors]]
            [clj-money.reports :as reports])
  (:use [clj-money.web.shared :refer :all]))

(defmacro with-accounts-layout
  [page-title entity-id options & content]
  `(with-layout
     ~page-title (assoc ~options :side-bar (budget-monitors ~entity-id))
     ~@content))

(defn- can-add-child?
  [account]
  true)

(defn- account-row
  "Renders a single account row"
  [account depth]
  [:tr
   [:td
    [:span {:class (format "account-depth-%s" depth)}
     (:name account)
     "&nbsp;"
     (when (can-add-child? account)
       [:a.small-add {:href (format "/entities/%s/accounts/new?parent-id=%s" (:entity-id account) (:id account))
                      :title "Click here to add a child to this account."}
        "+"])]]
   [:td.text-right
    [:span {:class (format "balance-depth-%s" depth)}
     (format-number (+ (:balance account) (:children-balance account)))]]
   [:td
    [:span.btn-group
     (glyph-button :list-alt
                   (format "/accounts/%s" (:id account))
                   {:level :default
                    :size :extra-small
                    :title "Click here to view transactions for this account"
                    :disabled (contains? (:tags account) :tradable)})
     (glyph-button :check
                   (format "/accounts/%s/reconciliations/new" (:id account))
                   {:level :default
                    :size :extra-small
                    :title "Click here to reconcile this account"})
     (glyph-button :pencil
                   (format "/accounts/%s/edit" (:id account))
                   {:level :info
                    :size :extra-small
                    :title "Click here to edit this account"})
     (glyph-button :remove
                   (format "/accounts/%s/delete" (:id account))
                   {:level :danger
                    :size :extra-small
                    :data-method :post
                    :data-confirm "Are you sure you want to delete this account?"
                    :title "Click here to remove this account"
                    :disabled (seq (:children account))})]]])

(defn- render-child-rows?
  [account]
  true)

(defn- account-and-children-rows
  "Renders an individual account row and any child rows"
  ([account] (account-and-children-rows account 0))
  ([account depth]
   (let [account-row (account-row account depth)]
     (if (render-child-rows? account)
       (concat
         [account-row]
         (->> (:children account)
              (map #(account-and-children-rows % (+ depth 1)))
              (into [])))
       account-row))))

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
  ([{{entity :entity :as params} :params} options]
   (authorize :index :account params)
   (with-accounts-layout "Accounts" (:id entity) (merge options {:entity entity})
     [:table.table.table-striped
      [:tr
       [:th.col-sm-6 "Name"]
       [:th.col-sm-4.text-right "Balance"]
       [:th.col-sm-2 "&nbsp;"]]
      (let [groups (accounts/select-nested-by-entity-id (env :db) (:id entity))]
        (map account-rows groups))]
     [:a.btn.btn-primary
      {:href (format "/entities/%s/accounts/new" (:id entity))
       :title "Click here to add a new account."}
      "Add"])))

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
   [:td.text-right (format-number polarized-amount)]
   [:td.text-right (format-number balance)]
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
     (glyph-button :paperclip
                   (-> (path "/transactions" transaction-id "attachments")
                       (query {:redirect (url-encode (format "/accounts/%s" account-id))})
                       format-url)
                   {:level :default
                    :size :extra-small
                    :title "Click here to view attachments for this transaction."})
     (let [can-delete? (->> transaction-id
                            (transactions/find-by-id (env :db))
                            transactions/can-delete?)]
       (glyph-button :remove
                     (-> (path "/transactions" transaction-id "delete")
                         (query {:redirect (url-encode (format "/accounts/%s" account-id))})
                         format-url)
                     {:level :danger
                      :disabled (not can-delete?)
                      :size :extra-small
                      :title (if can-delete?
                               "Click here to remove this transaction."
                               "This transaction contains reconciled items and cannot be removed.")
                      :data-method :post
                      :data-confirm "Are you sure you want to remove this transaction?"
                      :method :post}))]]])

(defmulti ^:private show-account
  (fn [account params]
    (cond
      (contains? (:tags account) :trading)
      :trading-account

      (contains? (:tags account) :tradable)
      :trading-detail

      :else
      :standard)))

(defmethod ^:private show-account :standard
  [account params]
  (html
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
                                         (:id account)
                                         (pagination/prepare-options params)))]
    [:p
     (pagination/nav
       (assoc params
              :url (-> (path "/accounts"
                             (:id account)))
              :total (transactions/count-items-by-account (env :db) (:id account))))]

    [:p
     [:a.btn.btn-primary
      {:href (-> (path "/entities"
                       (:entity-id account)
                       "transactions"
                       "new")
                 (query {:redirect (url-encode (format "/accounts/%s" (:id account)))})
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
      "Back"]]))

(defn- commodity-row
  [{:keys [style
           caption
           shares
           price
           cost
           gain
           value
           commodity-id]}
   account]
  [:tr {:class (format "report-%s" (name style))}
   [:td caption]
   [:td.text-right (if shares
                     (format-number shares {:format :commodity-price})
                     "&nbsp;")]
   [:td.text-right (if price
                     (format-number price {:format :commodity-price})
                     "&nbsp;")]
   [:td.text-right (format-number value)]
   [:td {:class (when gain (format "text-right %s" (if (<= 0 gain) "gain" "loss")))}
    (if gain
      (format-number gain)
      "&nbsp;")]
   [:td
    (when shares
      [:div.btn-group
       (glyph-button :list-alt
                     (format "/accounts/%s/lots?commodity-id=%s"
                             (:id account)
                             commodity-id)
                     {:size :extra-small
                      :title "Click here to view the lots for this commodity"})
       (glyph-button :plus-sign
                     (format "/accounts/%s/purchases/new?commodity-id=%s"
                             (:id account)
                             commodity-id)
                     {:size :extra-small
                      :level :success
                      :title "Click here to purchase more shares of this commodity."})
       (glyph-button :minus-sign
                     (format "/accounts/%s/sales/new?commodity-id=%s&shares=%s"
                             (:id account)
                             commodity-id
                             shares)
                     {:size :extra-small
                      :level :danger
                      :title "Click here to sell shares of this commodity."})])]])

(defmethod show-account :trading-account
  [account params]
  (html
    (let [summary (reports/commodities-account-summary (env :db) (:id account))]
      [:div.row
       [:div.col-md-10
        [:table.table.table-striped.table-hover
         [:tr
          [:th "Commodity"]
          [:th.text-right "Shares"]
          [:th.text-right "Price"]
          [:th.text-right "Value"]
          [:th.text-right "Gain"]
          [:th "&nbsp;"]]
         (if (seq summary)
           (map #(commodity-row % account)
                summary)
           [:tr
            [:td.empty-table {:colspan 4}
             "This account does not have any positions"]])]]])
    [:a.btn.btn-primary
     {:href (format "/accounts/%s/purchases/new" (:id account))
      :title "Click here to purchase a commodity with this account."}
     "Purchase"]
    "&nbsp;"
    [:a.btn.btn-default
     {:href (format "/entities/%s/accounts" (:entity-id account))
      :title "Click here to return to the list of accounts"}
     "Back"]))

(defmethod show-account :trading-detail
  [account params]
  (html
    [:p
     "Information page for commodity accounts is not ready yet."]
    [:a.btn.btn-primary
     {:href (format "/entities/%s/accounts" (:entity-id account))
      :title "Click here to return to the list of accounts."}
     "Back"]))

(defn show
  "Renders account details, including transactions"
  ([req] (show req {}))
  ([{{id :id :as params} :params} options]
   (let [account (accounts/find-by-id (env :db) id)]
     (authorize :show account params)
     (with-accounts-layout (format "Account - %s" (:name account)) (:entity-id account) options
       (show-account account params)))))

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

(defn- new-account-defaults
  [{:keys [entity-id parent-id]}]
  (let [parent (if parent-id
                 (accounts/find-by-id (env :db)  parent-id))
        account {:entity-id entity-id}]
    (if parent
      (-> account
          (assoc :parent-id (:id parent))
          (assoc :type (:type parent)))
      account)))

(defn new-account
  "Renders the new account form"
  ([{params :params :as req}]
   (new-account req (new-account-defaults params)))
  ([{params :params} account]
   (authorize :new :account params)
   (let [entity-id (:entity-id params)]
     (with-accounts-layout "New account" entity-id {}
       (form (format "/entities/%s/accounts" entity-id) {}
             (form-fields account))))))

(defn create
  "Creates the account and redirects to the index page on success, or
  re-renders the new form on failure"
  [{params :params}]
  (authorize :new :account params)
  (let [account (select-keys params [:entity-id :name :type :parent-id])
        saved (accounts/create (env :db) account)]
    (if (validation/has-error? saved)
      (new-account {:params (select-keys saved [:entity-id])} saved)
      (redirect (str "/entities/" (:entity-id params) "/accounts")))))

(defn edit
  "Renders the edit form for an account"
  [req]
  (let [account (or (:account req)
                    (accounts/find-by-id (env :db) (-> req :params :id)))]
    (authorize :edit account (:params req))
    (with-accounts-layout "Edit account" (:entity-id account) {}
      (form (format "/accounts/%s" (:id account)) {}
            [:input {:type :hidden
                     :name "entity-id"
                     :value (:entity-id account)}]
            (form-fields account)))))

(defn update
  "Updates the account and redirects to the account list on
  success or rerenders the edit from on error"
  [{params :params}]
  (let [account (accounts/find-by-id (env :db) (:id params))]
    (authorize :update account params)
    (let [updated (merge account
                         (select-keys params [:id
                                              :name
                                              :type
                                              :entity-id
                                              :parent-id]))
          result (accounts/update (env :db) updated)]
      (if (validation/has-error? result)
        (edit {:account result})
        (redirect (format "/entities/%s/accounts" (:entity-id result)))))))

(defn delete
  "Deletes the specified account"
  [{{id :id :as params} :params}]
  (let [account (accounts/find-by-id (env :db) id)]
    (authorize :delete account params)
    (try
      (accounts/delete (env :db) (:id account))
      (redirect (format "/entities/%s/accounts" (:entity-id account)))
      (catch Exception e
        (log/error e "Unable to delete account id=" id)
        (index (:entity-id account) {:alerts [{:type :danger
                                               :message (html [:strong "Unable to delete the account."]
                                                              "&nbsp;"
                                                              (.getMessage e))}]})))))
