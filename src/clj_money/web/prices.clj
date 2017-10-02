(ns clj-money.web.prices
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [ring.util.response :refer :all]
            [hiccup.core :refer :all]
            [clj-money.util :refer [format-date format-number]]
            [clj-money.web.shared :refer :all]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize
                                             tag-resource
                                             apply-scope]]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.prices.api-client :as prices-api]))

(defn- price-row
  [price]
  [:tr
   [:td.text-right (format-date (:trade-date price))]
   [:td.text-right (format-number (:price price))]
   [:td
    [:div.btn-group
     (glyph-button :remove
                   (format "/prices/%s/delete" (:id price))
                   {:level :danger
                    :size :extra-small
                    :title "Click here to remove this price"
                    :data-method :post
                    :data-confirm "Are you sure you want to remove this price?"})]]])

(defn index
  [{{commodity-id :commodity-id :as params} :params}]
  (let [commodity (commodities/find-by-id (env :db) commodity-id)
        criteria (apply-scope {:commodity-id commodity-id} :price)
        prices (prices/search (env :db) criteria)]
    (with-layout (format "Prices for %s" (:symbol commodity)) {}
      [:div.row
       [:div.col-md-3
        [:table.table.table-striped
         [:tr
          [:th.text-right "Trade date"]
          [:th.text-right "Price"]
          [:th "&nbsp;"]
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
         (form (format "/commodities/%s/prices" (:id commodity)) {}
               (date-input-field price :trade-date {:autofocus true})
               (number-input-field price :price)
               [:input.btn.btn-primary {:type :submit
                                        :title "Click here to save this price"}]
               "&nbsp;"
               [:a.btn.btn-default {:href (format "/commodities/%s/prices" (:id commodity))
                                    :title "Click here to return to the list of prices."}
                "Back"])]]))))

(defn create
  [{params :params}]
  (let [result (prices/create (env :db)
                              (select-keys params [:commodity-id
                                                   :trade-date
                                                   :price]))]
    (if (validation/has-error? result)
      (new-price {:params params} result)
      (redirect (format "/commodities/%s/prices" (:commodity-id result))))))

(defn delete
  [{params :params}]
  (let [price (prices/find-by-id (env :db) (Integer. (:id params)))]
    (prices/delete (env :db) (:id price))
    (redirect (format "/commodities/%s/prices" (:commodity-id price)))))

(defn fetch
  [{params :params}]
  (let [commodity (commodities/find-by-id (env :db) (:commodity-id params))
        price (prices-api/fetch commodity)
        to-create (-> {:commodity-id (:id commodity)
                       :price (:price price)
                       :trade-date (:trade-date price)}
                      (tag-resource :price)
                      (authorize :create))
        result (prices/create (env :db) to-create)]
    (if (validation/has-error? result)
      (new-price {:params params} result)
      (redirect (format "/commodities/%s/prices" (:commodity-id result))))))

(defn fetch-all
  [{params :params}]
  (->> (-> params
           (select-keys [:entity-id])
           (assoc :type ["stock" "fund"])
           (apply-scope :commodity))
       (commodities/search (env :db))
       (map #(-> %
                 prices-api/fetch
                 (assoc :commodity-id (:id %))))
       (map #(prices/create (env :db) %)))
  (redirect (format "/entities/%s/commodities" (:entity-id params))))
