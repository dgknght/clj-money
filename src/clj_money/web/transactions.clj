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
         [:th "Description"]]]
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
