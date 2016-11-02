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
   [:td.col-sm-2 (:transaction-date transaction)]
   [:td.col-sm-9 (:description transaction)]
   [:td.col-sm-1 (glyph-button :remove
                      (format "/transactions/%s/delete" (:id transaction))
                      {:level :danger
                       :size :extra-small
                       :data-method :post
                       :data-confirm "Are you sure you want to delete this transaction?"})]])

(defn index
  ([entity-id] (index entity-id {}))
  ([entity-id options]
   (layout
     "Transactions" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th.col-sm-2 "Date"]
         [:th.col-sm-9 "Description"]
         [:th.col-sm-1 "&nbsp;"]]
        (map transaction-row (transactions/select-by-entity-id (env :db) entity-id))]
      [:a.btn.btn-primary {:href (str"/entities/" entity-id "/transactions/new")} "Add"]]])))

(defn- item-row
  "Renders an individual row for a transaction item"
  [entity-id index item]
  [:tr
   [:td.col-sm-8
    (select-field item (str "account-id-" index) (account-options entity-id) {:suppress-label? true})]
   [:td.col-sm-2
    (text-input-field item (str "credit-amount-" index) {:suppress-label? true})]
   [:td.col-sm-2
    (text-input-field item (str "debit-amount-" index) {:suppress-label? true})]])

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
        (update-in [:account-id] #(Integer. %))
        (dissoc :credit-amount :debit-amount))))

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
         (map-indexed #(->> %2
                            ->form-item
                            (item-row entity-id %1)) (:items transaction))]
        [:input.btn.btn-primary {:type :submit :value "Save"}]]]])))

(defn- extract-items
  [params]
  (->> (iterate inc 0)
       (map (fn [index]
               (let [attr [:account-id :debit-amount :credit-amount]
                     indexed-attr (map #(keyword (str (name %) "-" index)) attr)
                     item (zipmap attr (map #(% params) indexed-attr))]
                 item))) 
       (take-while #(:account-id %))
       (map ->transaction-item)))

(defn create
  [params]
  (let [transaction (-> params
                        (assoc :items (extract-items params))
                        (select-keys [:entity-id :transaction-date :description :items])
                        (update-in [:items] (partial map #(select-keys % [:account-id :action :amount]))))
        result (transactions/create (env :db) transaction)]
    (if (validation/has-error? result)
      (new-transaction (:entity-id transaction) result {})
      (redirect (str "/entities/" (:entity-id result) "/transactions")))))

(defn delete
  [id]
  (let [transaction (transactions/find-by-id (env :db) id)]
    (transactions/delete (env :db) id)
    (redirect (str "/entities/" (:entity-id transaction) "/transactions"))))
