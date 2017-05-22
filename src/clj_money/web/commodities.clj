(ns clj-money.web.commodities
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [hiccup.core :refer :all]
            [clj-money.util :refer [format-number]]
            [clj-money.web.shared :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(defn- commodity-row
  [commodity]
  [:tr
   [:td (:symbol commodity)]
   [:td (:name commodity)]
   [:td (:exchange commodity)]
   [:td.text-right
    (format-number (:price (prices/most-recent (env :db)
                                       (:id commodity))))]
   [:td
    [:div.btn-group
     (glyph-button :pencil
                   (format "/commodities/%s/edit" (:id commodity))
                   {:level :info
                    :size :extra-small
                    :title "Click here to edit this commodity"})
     (glyph-button :refresh
                   (format "/commodities/%s/prices/fetch" (:id commodity))
                   {:level :default
                    :size :extra-small
                    :data-method :post
                    :title "Click here to fetch the latest price for this commodity"})
     (glyph-button :usd
                   (format "/commodities/%s/prices" (:id commodity))
                   {:level :default
                    :size :extra-small
                    :title "Click here to manage prices manually for this commodity"})
     (glyph-button :remove
                   (format "/commodities/%s/delete" (:id commodity))
                   {:level :danger
                    :size :extra-small
                    :data-method :post
                    :data-confirm "Are you sure you want to delete this account?"
                    :title "Click here to remove this commodity"})]]])

(defn index
  [{params :params}]
  (with-layout "Commodities" {}
    (let [entity-id (Integer. (:entity-id params))
          commodities (commodities/select-by-entity-id (env :db) entity-id)]
      [:div.row
       [:div.col-md-6
        [:table.table.table-striped
         [:tr
          [:th.col-md-3 "Symbol"]
          [:th.col-md-3 "Name"]
          [:th.col-md-2 "Exchange"]
          [:th.col-md-1.text-right "Last price"]
          [:th.col-md-3 "&nbsp;"]]
         (map commodity-row commodities)]
        [:a.btn.btn-primary
         {:href (format "/entities/%s/commodities/new" entity-id)
          :title "Click here to create a new commodity"}
         "Add"]
        "&nbsp;"
        [:a.btn.btn-default
         {:href (format "/entities/%s/prices/fetch" entity-id)
          :title "Click here to download prices for all commodities"
          :data-method :post}
         "Download Prices"]]])))

(defn- form-fields
  [commodity]
  (html
    [:div.row
     [:div.col-sm-6
      (text-input-field commodity :symbol {:autofocus true})]
     [:div.col-sm-6
      (select-field commodity :exchange (options-for-select commodities/exchanges name identity))]]
    [:div.row
     [:div.col-sm-12
      (text-input-field commodity :name)]]
    [:input.btn.btn-primary {:type :submit
                             :value "Save"
                             :title "Click here to save this commodity"}]
    "&nbsp;"
    [:a.btn.btn-default
     {:href (format "/entities/%s/commodities" (:entity-id commodity))
      :title "Click here to return to the list of commodities"}
     "Back"]))

(defn new-commodity
  ([{params :params :as req}]
   (new-commodity req {:entity-id (Integer. (:entity-id params))}))
  ([req commodity]
   (with-layout "New commodity" {}
     [:div.row
      [:div.col-md-6
       [:form {:action (format "/entities/%s/commodities" (:entity-id commodity))
               :method :post}
        (form-fields commodity)]]])))

(defn create
  [{params :params}]
  (let [commodity (commodities/create
                    (env :db)
                    (select-keys params [:entity-id :name :symbol :exchange]))]
    (if (validation/has-error? commodity)
      (new-commodity {} commodity)
      (redirect (format "/entities/%s/commodities" (:entity-id commodity))))))

(defn show
  [req]
  "show")

(defn edit
  ([{params :params :as req}]
   (let [id (Integer. (:id params))
         commodity (commodities/find-by-id (env :db) id)]
     (edit req commodity)))
  ([_ commodity]
   (with-layout "Edit commodity" {}
     [:div.row
      [:div.col-md-6
       [:form {:action (format "/commodities/%s" (:id commodity)) :method :post}
        (form-fields commodity)]]])))

(defn update
  [{params :params}]
  (let [result (commodities/update (env :db)
                                   (select-keys
                                     params
                                     [:id :name :symbol :exchange]))]
    (if (validation/has-error? result)
      (edit {} result)
      (redirect (format "/entities/%s/commodities" (:entity-id result))))))

(defn delete
  [{params :params}]
  (let [commodity (commodities/find-by-id (env :db) (Integer. (:id params)))]
    (commodities/delete (env :db) (:id commodity))
    (redirect (format "/entities/%s/commodities" (:entity-id commodity)))))
