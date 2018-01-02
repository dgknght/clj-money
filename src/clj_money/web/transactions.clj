(ns clj-money.web.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.format :refer [unparse-local-date formatters]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-time.core :as t]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             tag-resource
                                             apply-scope]]
            [clj-money.util :refer [ensure-local-date
                                    descending-periodic-seq]]
            [clj-money.url :refer :all]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.transactions]
            [clj-money.web.money-shared :refer [grouped-options-for-accounts
                                                budget-monitors
                                                available-month-options]]
            [clj-money.util :refer [format-date]])
  (:use [clj-money.web.shared :refer :all]))

(defmacro with-transactions-layout
  [page-title entity-or-id options & content]
  `(let [entity# (if (integer? ~entity-or-id)
                   (entities/find-by-id (env :db) ~entity-or-id)
                   ~entity-or-id)]
     (with-layout
       ~page-title (assoc ~options :side-bar (budget-monitors (:id entity#))
                          :entity entity#)
       ~@content)))

(defn- transaction-row
  "Renders a row in the transaction table"
  [transaction]
  [:tr
   [:td (format-date (:transaction-date transaction))]
   [:td (:description transaction)]
   [:td
    [:div.btn-group
     (when (allowed? :update transaction)
       (glyph-button :pencil
                     (format "/transactions/%s/edit" (:id transaction))
                     {:level :info
                      :size :extra-small
                      :title "Click here to edit this transaction."}))
     (when (allowed? :delete transaction)
       (let [can-delete? (transactions/can-delete? transaction)]
         (glyph-button :remove
                       (format "/transactions/%s/delete" (:id transaction))
                       {:level :danger
                        :disabled (not can-delete?)
                        :size :extra-small
                        :data-method :post
                        :data-confirm "Are you sure you want to delete this transaction?"
                        :title (if can-delete?
                                 "Click here to remove this transaction."
                                 "This transaction contains reconciled items and cannot be removed")})))]]])

(defn- filter-form
  [params]
  [:form {:action (format "/entities/%s/transactions" (:entity-id params))
          :method :get}
   [:div.input-group
    [:select.form-control {:name "transaction-date"
                           :id "transaction-date" }
     (available-month-options (:transaction-date params))]
    [:span.input-group-btn
     [:button.btn.btn-default {:type :submit}
      "Search"]]]])

(defn index
  ([req] (index req {}))
  ([{{entity-id :entity-id :as params} :params} options]
   (with-transactions-layout "Transactions" entity-id options
     (let [transaction-date (or (:transaction-date params)
                                (unparse-local-date
                                  (:year-month formatters)
                                  (t/today)))
           criteria (apply-scope {:entity-id entity-id
                                  :transaction-date transaction-date}
                                 :transaction)
           transactions (transactions/search
                          (env :db)
                          criteria
                          {:limit 1000
                           :sort [[:transaction-date :desc]]})]
       (html
         (filter-form (-> params
                          (select-keys [:entity-id])
                          (assoc :transaction-date transaction-date)))
         [:table.table.table-striped
          [:tr
           [:th.col-sm-2 "Date"]
           [:th.col-sm-8 "Description"]
           [:th.col-sm-2 "&nbsp;"]]
          (map transaction-row transactions)]))
     (when (allowed? :create (-> {:entity-id entity-id}
                                 (tag-resource :transaction)))
       [:p
        [:a.btn.btn-primary
         {:href (str"/entities/" entity-id "/transactions/new")
          :title "Click here to enter a new transaction."}
         "Add"]]))))

(defn- item-row
  "Renders an individual row for a transaction item"
  [entity-id index item]
  [:tr
   [:td.col-sm-4
    (html
      (when-let [id (:id item)]
        (hidden-input-element (str "id-" index) id))
      (select-element (str "account-id-" index)
                      (:account-id item)
                      (grouped-options-for-accounts entity-id
                                                    {:include-none? true
                                                     :selected-id (:account-id item)})
                      {:suppress-label? true}))]
   [:td.col-sm-4
    (text-input-element (str "memo-" index) (:memo item) {:suppress-label? true}) ]
   [:td.col-sm-2
    (text-input-element (str "credit-amount-" index) (:credit-amount item) {:suppress-label? true})]
   [:td.col-sm-2
    (text-input-element (str "debit-amount-" index) (:debit-amount item) {:suppress-label? true})]])

(defn- ->form-item
  "Tranforms a transaction item as managed by the system into a
  item that can be used by the form"
  [transaction-item]
  (-> transaction-item
      (assoc :credit-amount (when (= :credit (:action transaction-item))
                              (:amount transaction-item))
             :debit-amount (when (= :debit (:action transaction-item))
                             (:amount transaction-item)))
      (dissoc :amount :action)))

(defn- ->transaction-item
  "Transforms a form item into a transaction item"
  [{:keys [credit-amount debit-amount] :as item}]
  (let [[action amount] (if (seq credit-amount)
                          [:credit (bigdec credit-amount)]
                          [:debit (bigdec debit-amount)])]
    (-> item
        (assoc :action action
               :amount amount)
        (dissoc :credit-amount :debit-amount))))

(defn- items-for-form
  [transaction]
  (map-indexed #(->> %2
                        ->form-item
                        (item-row (:entity-id transaction) %1))
               (concat (:items transaction)
                       (repeat {:action nil
                                :account-id nil
                                :amount nil}))))

(defn- form-fields
  [transaction back-url]
  (html
    [:div.row
     [:div.col-md-6
      (date-input-field transaction :transaction-date)]
     [:div.col-md-6
      (text-input-field transaction :description)]]
    [:div.row
     [:div.col-md-12
      (text-input-field transaction :memo)]]
    [:table.table.table-striped
     [:tr
      [:th "Account"]
      [:th "Memo"]
      [:th "Credit"]
      [:th "Debit"]]
     (take 10 (items-for-form transaction))]
    [:input.btn.btn-primary {:type :submit :value "Save"}]
    "&nbsp;"
    [:a.btn.btn-default
     {:href back-url
      :title "Click here to return to the list of transactions"}
     "Back"]))

(defn- validate-redirect-url
  [url]
  (when (and url (not (re-matches #"\Ahttps?:://" url)))
    url))

(defn- redirect-url
  [entity-id params]
  (or (validate-redirect-url (:redirect params))
      (format "/entities/%s/transactions" entity-id)))

(defn new-transaction
  ([{params :params}]
   (new-transaction params
                    (-> {:entity-id (:entity-id params)
                         :items [{:action :debit}
                                 {:action :credit}]
                         :transaction-date (t/today)}
                        (tag-resource :transaction)
                        (authorize :new))
                    {}))
  ([params transaction options]
   (with-transactions-layout "New Transaction" (:entity-id transaction) options
     (form (cond-> (path "/entities"
                         (:entity-id transaction)
                         "transactions")
             (contains? params :redirect) (query {:redirect (-> params
                                                                :redirect
                                                                url-encode)})
             true format-url) {}
           (form-fields transaction (redirect-url (:entity-id transaction) params))))))

(defn- valid-item?
  [{account-id :account-id :as item}]
  (or (integer? account-id)
      (and (string? account-id)
           (seq account-id))))

(defn- extract-items
  [params]
  (->> (iterate inc 0)
       (map (fn [index]
               (let [attr [:id :account-id :debit-amount :credit-amount :memo]
                     indexed-attr (map #(keyword (str (name %) "-" index)) attr)
                     item (zipmap attr (map #(% params) indexed-attr))]
                 item))) 
       (take-while valid-item?)
       (map ->transaction-item)))

(defn create
  [{params :params}]
  (let [transaction (-> params
                        (assoc :items (extract-items params))
                        (select-keys [:entity-id :transaction-date :description :items :memo])
                        (update-in [:items] (partial map #(select-keys % [:account-id :action :amount :memo])))
                        (tag-resource :transaction)
                        (authorize :create))
        result (transactions/create (env :db) transaction)
        redirect-url (redirect-url (:entity-id result) params)]
    (if (validation/has-error? result)
      (new-transaction params result {})
      (redirect redirect-url))))

(defn edit
  ([req] (edit req {}))
  ([{params :params transaction :transaction} options]
   (let [id (:id params)
         transaction (or transaction
                         (authorize (transactions/find-by-id (env :db) id) :update))
         action (cond-> (path "/transactions"
                              (:id transaction))

                  (:redirect params)
                  (query {:redirect (url-encode (:redirect params))})

                  true
                  format-url)]
     (with-transactions-layout "New Transaction" (:entity-id transaction) options
       (form action {}
             (form-fields transaction (redirect-url (:entity-id transaction) params)))))))

(defn update
  [{params :params}]
  (let [transaction (authorize (transactions/find-by-id
                                 (env :db)
                                 (:id params)
                                 (ensure-local-date (:original-transaction-date params)))
                               :update)
        updated (merge transaction
                       (-> params
                           (select-keys [:id
                                         :transaction-date
                                         :original-transaction-date
                                         :description
                                         :memo])
                           (assoc :items (extract-items params))))
        result (transactions/update (env :db) updated)
        redirect-url (redirect-url (:entity-id result) params)]
    (if (validation/has-error? result)
      (edit result {:alerts [{:type :danger :message (str "Unable to save the transaction " (validation/error-messages updated))}]})
      (redirect redirect-url))))

(defn delete
  [{{id :id :as params} :params}]
  (let [transaction (authorize (transactions/find-by-id (env :db) id) :delete)
        redirect-url (redirect-url (:entity-id transaction) params)]
    (transactions/delete (env :db) id)
    (redirect redirect-url)))
