(ns clj-money.web.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.util :refer [format-date
                                    format-number]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]
            [clj-money.models.lot-transactions :as lot-transactions]
            [clj-money.reports :as reports]
            [clj-money.web.shared :refer :all]
            [clj-money.web.money-shared :refer [budget-monitors]]))

(defmacro with-lots-layout
  [page-title entity-id options & content]
  `(with-layout
     ~page-title (assoc ~options :side-bar (budget-monitors (Integer. ~entity-id)))
     ~@content))

(defn- lot-transaction-row
  [record]
  [:tr
   [:td.text-right (-> record :trade-date format-date)]
   [:td.text-right (format "%s%s"
                           (if (= :buy (:action record)) "+ " "- ")
                           (-> record :shares format-number))]
   [:td.text-right (-> record :price format-number)]
   [:td.text-right (-> record :value format-number)]
   [:td.text-right (-> record :gain format-number)]
   [:td.text-center
    [:div.btn-group
     (if (= :buy (:action record))
       (glyph-button :remove
                     (format "/transactions/%s/unbuy" (:transaction-id record))
                     {:size :extra-small
                      :data-method :post
                      :data-confirm "Are you sure you want to undo this purchase?"
                      :level :danger
                      :title "Click here to undo this purchase"})
       (glyph-button :remove
                     (format "/transactions/%s/unsell" (:transaction-id record))
                     {:size :extra-small
                      :data-method :post
                      :data-confirm "Are you sure you want to undo this sale?"
                      :level :danger
                      :title "Click here to undo this sale."}))]]])

(defn- lot-transactions-table
  [records]
  [:table.table.table-striped
   [:tr
    [:th.text-right "Trade Date"]
    [:th.text-right "Shares"]
    [:th.text-right "Price"]
    [:th.text-right "Value"]
    [:th.text-right "Gain"]
    [:th.text-right "&nbsp;"]]
   (map lot-transaction-row records)])

(defn- lot-row
  [record]
  (html
    [:tr
     [:td.text-right (-> record :shares-owned format-number)]
     [:td.text-right (-> record :purchase-price format-number)]
     [:td.text-right (-> record :current-price format-number)]
     [:td.text-right (-> record :cost format-number)]
     [:td.text-right (-> record :value format-number)]
     [:td.text-right
      {:class (if (<= 0 (:gain record)) "gain" "loss")}
      (-> record :gain format-number)]]))

(defn- lot-section
  [record]
  (html
    [:div.row
     [:div.col-sm-12
      [:table.table.table-striped
       [:tr
        [:th.text-right "Shares Owned"]
        [:th.text-right "Purchase Price"]
        [:th.text-right "Current Price"]
        [:th.text-right "Cost"]
        [:th.text-right "Value"]
        [:th.text-right "Gain"]]
       (lot-row record)]]]
    [:div.row
     [:div.col-sm-10.col-sm-offset-2
      (lot-transactions-table (:lot-transactions record))]]))

(defn index
  [{{:keys [account-id commodity-id]} :params}]
  (let [account (accounts/find-by-id (env :db) (Integer. account-id))
        commodity (commodities/find-by-id (env :db) (Integer. commodity-id))
        records (reports/lot-report (env :db) (:id account) (:id commodity))]
    (with-lots-layout (format "Lots of %s in %s" (:symbol commodity) (:name account)) (:entity-id account) {}
      (map lot-section records)
      [:a.btn.btn-default {:href (format "/accounts/%s" (:id account))
                           :title "Click here to return to the account page"}
       "Back"])))
