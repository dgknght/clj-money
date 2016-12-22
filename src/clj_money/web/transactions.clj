(ns clj-money.web.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-time.core :as t]
            [clj-money.url :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.web.money-shared :refer [account-options]]
            [clj-money.util :refer [format-date]]
            [clj-money.schema :as schema])
  (:use [clj-money.web.shared :refer :all]))

(defn- transaction-row
  "Renders a row in the transaction table"
  [transaction]
  [:tr
   [:td (:transaction-date transaction)]
   [:td (:description transaction)]
   [:td
    [:div.btn-group
     (glyph-button :pencil
                   (format "/transactions/%s/edit" (:id transaction))
                   {:level :info
                    :size :extra-small
                    :title "Click here to edit this transaction."})
     (glyph-button :remove
                   (format "/transactions/%s/delete" (:id transaction))
                   {:level :danger
                    :size :extra-small
                    :data-method :post
                    :data-confirm "Are you sure you want to delete this transaction?"
                    :title "Click here to remove this transaction."})]]])

(defn index
  ([entity-id] (index entity-id {}))
  ([entity-id options]
   (with-layout "Transactions" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th.col-sm-2 "Date"]
         [:th.col-sm-8 "Description"]
         [:th.col-sm-2 "&nbsp;"]]
        (map transaction-row (transactions/select-by-entity-id (env :db) entity-id))]
       [:a.btn.btn-primary
        {:href (str"/entities/" entity-id "/transactions/new")
         :title "Click here to enter a new transaction."}
        "Add"]]])))

(defn- item-row
  "Renders an individual row for a transaction item"
  [entity-id index item]
  [:tr
   [:td.col-sm-8
    (html
      (when-let [id (:id item)]
        (hidden-input-element (str "id-" index) id))
      (select-element (str "account-id-" index)
                      (:account-id item)
                      (account-options entity-id
                                       {:include-none? true})
                      {:suppress-label? true}))]
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
        (update-in [:account-id] #(Integer. %))
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
    (text-input-field transaction
                      :transaction-date
                      {:autofocus true
                       :class "date-field"
                       :format-fn format-date})
    (text-input-field transaction :description)
    [:table.table.table-striped
     [:tr
      [:th "Account"]
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
  ([params]
   (new-transaction params
                    {:entity-id (:entity-id params)
                     :items [{:action :debit}
                             {:action :credit}]
                     :transaction-date (t/today)}
                    {}))
  ([params transaction options]
   (with-layout "New Transaction" options
     [:div.row
      [:div.col-md-6
       [:form {:action (cond-> (path "/entities"
                                     (:entity-id params)
                                     "transactions")
                         (contains? params :redirect) (query {:redirect (-> params
                                                                            :redirect
                                                                            url-encode)})
                         true format-url)
               :method :post}
        (form-fields transaction (redirect-url (:entity-id params) params))]]])))

(defn- valid-item?
  [{account-id :account-id :as item}]
  (or (integer? account-id)
      (and (string? account-id)
           (seq account-id))))

(defn- extract-items
  [params]
  (->> (iterate inc 0)
       (map (fn [index]
               (let [attr [:id :account-id :debit-amount :credit-amount]
                     indexed-attr (map #(keyword (str (name %) "-" index)) attr)
                     item (zipmap attr (map #(% params) indexed-attr))]
                 item))) 
       (take-while valid-item?)
       (map ->transaction-item)))

(defn create
  [params]
  (let [transaction (-> params
                        (assoc :items (extract-items params))
                        (select-keys [:entity-id :transaction-date :description :items])
                        (update-in [:items] (partial map #(select-keys % [:account-id :action :amount]))))
        result (transactions/create (env :db) transaction)
        redirect-url (redirect-url (:entity-id result) params)]
    (if (validation/has-error? result)
      (new-transaction (:entity-id transaction) result {})
      (redirect redirect-url))))

(defn edit
  ([id-or-trans] (edit id-or-trans {}))
  ([id-or-trans options]
   (with-layout "New Transaction" options
     [:div.row
      [:div.col-md-6
       (let [transaction (if (map? id-or-trans)
                           id-or-trans
                           (transactions/find-by-id (env :db) id-or-trans))
             action (cond-> (path "/transactions"
                                  (:id transaction))

                      (:redirect options)
                      (query {:redirect (url-encode (:redirect options))})

                      true
                      format-url)]
         [:form {:action action
                 :method :post}
          (form-fields transaction (redirect-url (:entity-id transaction) options))])]])))

(defn update
  [params]
  (let [transaction (-> params
                        (select-keys [:id
                                      :transaction-date
                                      :description])
                        (assoc :items (extract-items params)))
        updated (transactions/update (env :db) transaction)
        redirect-url (redirect-url (:entity-id updated) params)]
    (if (validation/has-error? updated)
      (edit transaction {:alerts [{:type :danger :message (str "Unable to save the transaction " (validation/get-errors updated))}]})
      (redirect redirect-url))))

(defn delete
  [{id :id :as params}]
  (let [transaction (transactions/find-by-id (env :db) id)
        redirect-url (redirect-url (:entity-id transaction) params)]
    (transactions/delete (env :db) id)
    (redirect redirect-url)))
