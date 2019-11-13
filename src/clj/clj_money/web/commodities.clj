(ns clj-money.web.commodities
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [hiccup.core :refer [html]]
            [ring.util.response :refer [redirect]]
            [clj-money.util :refer [format-number]]
            [clj-money.web.shared :refer [append-anti-forgery-link-attributes
                                          form
                                          glyph-button
                                          options-for-select
                                          select-field
                                          text-input-field
                                          with-layout]]
            [clj-money.authorization :refer [apply-scope
                                             allowed?
                                             authorize
                                             tag-resource]]
            [clj-money.permissions.commodities]
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
     (when (allowed? :update commodity)
       (glyph-button :pencil
                     (format "/commodities/%s/edit" (:id commodity))
                     {:level :info
                      :size :extra-small
                      :title "Click here to edit this commodity"}))
     ; TODO check :index permission for prices
     (glyph-button :usd
                   (format "/commodities/%s/prices" (:id commodity))
                   {:level :default
                    :size :extra-small
                    :title "Click here to manage prices manually for this commodity"})
     (when (allowed? :delete commodity)
       (glyph-button :remove
                     (format "/commodities/%s/delete" (:id commodity))
                     {:level :danger
                      :size :extra-small
                      :data-method :post
                      :data-confirm "Are you sure you want to delete this account?"
                      :title "Click here to remove this commodity"}))
     (when (and (#{:stock :fund} (:type commodity))
                (allowed? :create (-> {:commodity-id (:id commodity)}
                                      (tag-resource :price))))
       (glyph-button :refresh
                     (format "/commodities/%s/prices/fetch" (:id commodity))
                     {:level :default
                      :size :extra-small
                      :data-method :post
                      :title "Click here to fetch the latest price for this commodity"}))]]])

(defn index
  [{params :params}]
  (let [entity (:entity params)
        criteria (apply-scope {:entity-id (:id entity)} :commodity)
        commodities (sort-by :symbol (commodities/search (env :db) criteria))]
    (with-layout "Commodities" {:entity entity}
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
        (when (allowed? :create (-> {:entity-id (:id entity)}
                                    (tag-resource :commodity)))
          [:a.btn.btn-primary
           {:href (format "/entities/%s/commodities/new" (:id entity))
            :title "Click here to create a new commodity"}
           "Add"])
        "&nbsp;"
        ; TODO make sure we can create price resources
        [:a.btn.btn-default
         (append-anti-forgery-link-attributes
           {:href (format "/entities/%s/prices/fetch" (:id entity))
            :title "Click here to download prices for all commodities"
            :data-method :post})
         "Download Prices"]]])))

(defn- form-fields
  [commodity]
  (html
    [:div.row
     [:div.col-sm-6
      (select-field commodity :type (options-for-select [:currency :stock] name identity))]
     [:div.col-sm-6
      (select-field commodity :exchange (options-for-select commodities/exchanges name identity))]]
    [:div.row
     [:div.col-sm-6
      (text-input-field commodity :name {:autofocus true})]
     [:div.col-sm-6
      (text-input-field commodity :symbol)]]
    [:input.btn.btn-primary {:type :submit
                             :value "Save"
                             :title "Click here to save this commodity"}]
    "&nbsp;"
    [:a.btn.btn-default
     {:href (format "/entities/%s/commodities" (:entity-id commodity))
      :title "Click here to return to the list of commodities"}
     "Back"]))

(defn new-commodity
  ([{{entity-id :entity-id} :params :as req}]
   (new-commodity req (-> {:entity-id entity-id}
                          (tag-resource :commodity)
                          (authorize :create))))
  ([{{entity :entity} :params} commodity]
   (with-layout "New commodity" {:entity entity}
     [:div.row
      [:div.col-md-6
       (form (format "/entities/%s/commodities" (:entity-id commodity)) {}
             (form-fields commodity))]])))

(defn create
  [{params :params}]
  (let [commodity (commodities/create
                    (env :db)
                    (-> params
                        (select-keys [:entity-id
                                      :name
                                      :symbol
                                      :exchange
                                      :type])
                        (tag-resource :commodity)
                        (authorize :create)))]
    (if (validation/has-error? commodity)
      (new-commodity {} commodity)
      (redirect (format "/entities/%s/commodities" (:entity-id commodity))))))

(defn edit
  ([{params :params :as req}]
   (let [id (:id params)
         commodity (authorize (commodities/find-by-id (env :db) id) :update)]
     (edit req commodity)))
  ([_ commodity]
   (with-layout "Edit commodity" {:entity-id (:entity-id commodity)}
     [:div.row
      [:div.col-md-6
       (form (format "/commodities/%s" (:id commodity)) {}
             (form-fields commodity))]])))

(defn update
  [{params :params}]
  (let [commodity (-> (commodities/find-by-id (env :db) (:id params))
                      (authorize :update)
                      (merge (select-keys params [:id
                                                  :name
                                                  :symbol
                                                  :exchange
                                                  :type])))
        result (commodities/update (env :db) commodity)]
    (if (validation/has-error? result)
      (edit {} result)
      (redirect (format "/entities/%s/commodities" (:entity-id result))))))

(defn delete
  [{params :params}]
  (let [commodity (authorize (commodities/find-by-id (env :db) (:id params)) :delete)]
    (commodities/delete (env :db) (:id commodity))
    (redirect (format "/entities/%s/commodities" (:entity-id commodity)))))
