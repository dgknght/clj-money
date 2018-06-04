(ns clj-money.commodities
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [clj-money.data :as data]
            [clj-money.notifications :as notify]
            [clj-money.entities :as entities]
            [clj-money.forms :refer [text-input
                                     required]]))

; TODO need to empty this on change of entity, and probably also when navigating away
(defonce commodities (r/atom []))

(defn- load-commodities
  []
  (data/get-commodities (:id entities/current) #(reset! commodities %)))

(defn- commodity-row
  [commodity]
  ^{:key (:id commodity)}
  [:tr
   [:td (:name commodity)]
   [:td (:symbol commodity)]
   [:td (:exchange commodity)]
   [:td "coming soon..."]])

(defn management
  "Renders the commodity management UI"
  []
  (load-commodities)
  (fn []
    [:section
     [:h1 "Commodities"]
     [:table.table.table-stripped.table-hover
      [:tbody
       [:tr
        [:th "Name"]
        [:th "Symbol"]
        [:th "Exchange"]
        [:th "Latest Price"]]
       (map commodity-row @commodities)]]]))
