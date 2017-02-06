(ns clj-money.web.commodities
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [clj-money.web.shared :refer :all]
            [clj-money.validation :as validation]
            [clj-money.models.commodities :as commodities]))

(defn- commodity-row
  [commodity]
  [:tr
   [:td (:symbol commodity)]
   [:td (:name commodity)]
   [:td (:exchange commodity)]])

(defn index
  [{params :params}]
  (with-layout "Commodities" {}
    (let [entity-id (Integer. (:entity-id params))
          commodities (commodities/select-by-entity-id (env :db) entity-id)]
      [:div.row
       [:div.col-md-6
        [:table.table.table-striped
         [:tr
          [:th "Symbol"]
          [:th "Name"]
          [:th "Exchange"]]
         (map commodity-row commodities)]
        [:a.btn.btn-primary
         {:href (format "/entities/%s/commodities/new" entity-id)
          :title "Click here to create a new commodity"}
         "Add"]]])))

(defn new-commodity
  ([{params :params :as req}]
   (new-commodity req {:entity-id (Integer. (:entity-id params))}))
  ([req commodity]
   (with-layout "New commodity" {}
     [:div.row
      [:div.col-md-6

       [:pre (prn-str (validation/error-messages commodity))]

       [:form {:action (format "/entities/%s/commodities" (:entity-id commodity))
               :method :post}
        [:div.row
         [:div.col-sm-6
          (text-input-field commodity :symbol)]
         [:div.col-sm-6
          (select-field commodity :exchange (options-for-select commodities/exchanges name identity))]]
        [:div.row
         [:div.col-sm-12
          (text-input-field commodity :name)]]
        [:input.btn.btn-primary {:type :submit
                                 :value "Save"
                                 :title "Click here to save this commodity"}]]]])))

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
  [req]
  "edit")

(defn update
  [req]
  "update")

(defn delete
  [req]
  "delete")
