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
            [clj-money.web.shared :refer :all]
            [clj-money.web.money-shared :refer [budget-monitors]]))

(defmacro with-lots-layout
  [page-title entity-id options & content]
  `(with-layout
     ~page-title (assoc ~options :side-bar (budget-monitors (Integer. ~entity-id)))
     ~@content))

(defn- lot-row
  [lot]
  (let [purchase-tx (->> {:lot-id (:id lot)}
                            (lot-transactions/select (env :db))
                            (filter #(= :buy (:action %)))
                            first)
        purchase-price (:price purchase-tx)
        shares-owned (:shares-owned lot)
        current-price (:price (prices/most-recent (env :db) (:commodity-id lot)))
        gain (* shares-owned (- purchase-price current-price))]
    [:tr
     [:td.text-right (-> lot :purchase-date format-date)]
     [:td.text-right (-> lot :shares-owned format-number)]
     [:td.text-right (format-number purchase-price)]
     [:td.text-right (format-number current-price)]
     [:td.text-right (format-number (* current-price (:shares-owned lot)))]
     [:td.text-right {:class (if (<= gain 0) "gain" "loss")} (format-number gain)]
     [:td
      [:div.btn-group
       (glyph-button :remove
                     (format "/transactions/%s/unbuy" (:transaction-id purchase-tx))
                     {:size :extra-small
                      :data-method :post
                      :data-confirm "Are you sure you want to undo this purchase?"
                      :level :danger
                      :title "Click here to undo this purchase"})]]]))

(defn index
  [{{:keys [account-id commodity-id]} :params}]
  (let [account (accounts/find-by-id (env :db) (Integer. account-id))
        commodity (commodities/find-by-id (env :db) (Integer. commodity-id))]
    (with-lots-layout (format "Lots of %s in %s" (:symbol commodity) (:name account)) (:entity-id account) {}
      [:table.table.table-striped
       [:tr
        [:th "Purchase Date"]
        [:th "Shares Owned"]
        [:th "Purchase Price"]
        [:th "Current Price"]
        [:th "Value"]
        [:th "Gain"]
        [:th "&nbsp;"]]
       (let [l (lots/search (env :db) {:account-id (:id account)
                                       :commodity-id (:id commodity)})]
         (html
           (map lot-row l)
           [:tr.report-summary
            [:td.text-right {:colspan 2} (->> l
                                              (map :shares-owned)
                                              (reduce +)
                                              format-number)]
            [:td.text-right {:colspan 5} "&nbsp;"]]))]
      [:a.btn.btn-default {:href (format "/accounts/%s" (:id account))
                           :title "Click here to return to the account page"}
       "Back"])))
