(ns clj-money.web.prices
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [hiccup.core :refer :all]
            [clj-money.util :refer [format-date format-number]]
            [clj-money.web.shared :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(defn- price-row
  [price]
  [:tr
   [:td (format-date (:trade-date price))]
   [:td (format-number (:price price))]])

(defn index
  [{params :params}]
  (let [commodity (commodities/find-by-id (env :db)
                                          (Integer. (:commodity-id params)))
        prices (prices/select-by-commodity-id (env :db) (:id commodity))]
    (with-layout (format "Prices for %s" (:symbol commodity)) {}
      [:div.row
       [:div.col-md-6
        [:table.table.table-striped
         [:tr
          [:th "Trade date"]
          [:th "Price"]
          (map price-row prices)]]
        [:a.btn.btn-primary
         {:href (format "/commodities/%s/prices/new" (:id commodity))
          :title "Click here to add a new price"}
         "Add"]
        "&nbsp;"
        [:a.btn.btn-default
         {:href (format "/entities/%s/commodities" (:entity-id commodity))
          :title "Click here to return to the list of commodities"}
         "Back"]]])))

(defn new-price
  ([{params :params :as req}]
   (new-price req {:commodity-id (Integer. (:commodity-id params))}))
  ([{params :params} price]
   (let [commodity (commodities/find-by-id (env :db) (Integer. (:commodity-id params)))]
     (with-layout (format "New price for %s" (:symbol commodity)) {}
       [:div.row
        [:div.col-md-6
         [:form {:action (format "/commodities/%s/prices" (:id commodity))
                 :method :post}
          (date-input-field price :trade-date {:autofocus true})
          (number-input-field price :price)
          [:input.btn.btn-primary {:type :submit
                                   :title "Click here to save this price"}]
          "&nbsp;"
          [:a.btn.btn-default {:href (format "/commodities/%s/prices" (:id commodity))
                               :title "Click here to return to the list of prices."}
           "Back"]]]]))))

(defn create
  [{params :params}]
  (let [result (prices/create (env :db)
                              (select-keys params [:commodity-id
                                                   :trade-date
                                                   :price]))]
    (if (validation/has-error? result)
      (new-price {:params params} result)
      (redirect (format "/commodities/%s/prices" (:commodity-id result))))))

(defn show
  [req]
  "show")

(defn edit
  [req]
  "edit")

(defn update
  [req]
  "update")

(defn delete
  [req]
  "delete")
