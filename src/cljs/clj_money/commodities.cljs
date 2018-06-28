(ns clj-money.commodities
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.data :as data]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.util :as util]
            [clj-money.forms :refer [text-input
                                     select-input
                                     required]]))

(defn- commodity-row
  [commodity]
  ^{:key (:id commodity)}
  [:tr
   [:td (:name commodity)]
   [:td (:symbol commodity)]
   [:td (:exchange commodity)]
   [:td "coming soon..."]])

(defn- commodity-list []
  (let [commodities (r/atom [])]
    (data/get-commodities (:id @state/current-entity)
                          #(reset! commodities %))
    (fn []
      [:table.table.table-striped.table-hover
       [:tbody
        [:tr
         [:th "Name"]
         [:th "Symbol"]
         [:th "Exchange"]
         [:th "Latest Price"]]
        (for [commodity @commodities]
          (commodity-row commodity))]])))

(defn- commodities-page []
  (with-layout
    [:section
     [:h1 "Commodities"]
     [commodity-list]
     (util/add-button "/commodities/new")]))

(defn- create-commodity
  [commodity]
  (.log js/console "create commodity " (prn-str commodity)))

(def ^:private commodity-form
  [:form
   (select-input :type ["currency" "stock"])
   (select-input :exchange ["nyse" "nasdaq"] {:visible? #(= (:type %) "stock")})
   (text-input :symbol :required)
   (text-input :name :required)])

(defn- new-commodity []
  (let [commodity (r/atom {})]
  (with-layout
    [:div.row
     [:div.col-md-6
     [:h1 "New Commodity"]
     [bind-fields commodity-form commodity]
     (util/button "Save" #(create-commodity @commodity) {:class "btn btn-primary"
                                                         :icon :ok})
     (util/space)
     (util/link-to "Cancel" "/commodities" {:class "btn btn-danger"
                                            :icon :ban-circle})]])))

(secretary/defroute "/commodities/new" []
  (r/render [new-commodity] (app-element)))

(secretary/defroute "/commodities" []
  (r/render [commodities-page] (app-element)))
