(ns clj-money.web.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.web.money-shared :refer [account-options]]
            [clj-money.schema :as schema])
  (:use [clj-money.web.shared :refer :all]))

(defn- transaction-row
  "Renders a row in the transaction table"
  [transaction]
  [:tr
   [:td (:transaction-date transaction)]
   [:td (:description transaction)]])

(defn index
  ([entity-id] (index entity-id {}))
  ([entity-id options]
   (layout
     "Transactions" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th "Date"]
         [:th "Description"]]
        (map transaction-row (transactions/select-by-entity-id (env :db) entity-id))]
      [:a.btn.btn-primary {:href (str"/entities/" entity-id "/transactions/new")} "Add"]]])))

(defn- item-row
  "Renders an individual row for a transaction item"
  [entity-id item]
  [:tr
   [:td.col-sm-8
    (select-field item :account-id (account-options entity-id) {:suppress-label? true})]
   [:td.col-sm-2
    (text-input-field item :credit-amount {:suppress-label? true})]
   [:td.col-sm-2
    (text-input-field item :debit-amount {:suppress-label? true})]])

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
  (-> item
      (assoc :action (if credit-amount :credit :debit)
             :amount (if credit-amount credit-amount debit-amount))
      (dissoc :credit-amount :debit-amount)))

(defn new-transaction
  ([entity-id] (new-transaction entity-id
                                {:entity-id entity-id
                                 :items [{:action :debit}
                                         {:action :credit}]}
                                {}))
  ([entity-id transaction options]
   (layout
     "New Transaction" options
     [:div.row
      [:div.col-md-6
       [:form {:action (str "/entities/" entity-id "/transactions") :method :post}
        (text-input-field transaction :transaction-date {:autofocus true
                                                         :class "date-field"})
        (text-input-field transaction :description)
        [:table.table.table-striped
         [:tr
          [:th "Account"]
          [:th "Credit"]
          [:th "Debit"]]
         (map #(->> %
                    ->form-item
                    (item-row entity-id)) (:items transaction))]
        [:input.btn.btn-primary {:type :submit :value "Save"}]]]])))

(defn create
  [params]
  (let [transaction (-> params
                        (select-keys [:entity-id :transaction-date :description :items])
                        (update-in [:items] (partial map ->transaction-item))
                        (update-in [:items] (partial map #(select-keys % [:account-id :action :amount]))))
        result (transactions/create (env :db) transaction)]
    (if (validation/has-error? result)
      (new-transaction (:entity-id transaction) result {})
      (redirect (str "/entities/" (:entity-id result) "/transactions")))))
