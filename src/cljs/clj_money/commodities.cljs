(ns clj-money.commodities
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.data :as data]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.forms :refer [text-input
                                     required]]))

; TODO need to empty this on change of entity, and probably also when navigating away
(defonce commodities (r/atom []))

(defn- load-commodities
  []
  (data/get-commodities (:id @state/current-entity) #(reset! commodities %)))

(defn- commodity-row
  [commodity]
  ^{:key (:id commodity)}
  [:tr
   [:td (:name commodity)]
   [:td (:symbol commodity)]
   [:td (:exchange commodity)]
   [:td "coming soon..."]])

(defn- commodity-list []
  #_(load-commodities)
  (fn []
    [:table.table.table-stripped.table-hover
     [:tbody
      [:tr
       [:th "Name"]
       [:th "Symbol"]
       [:th "Exchange"]
       [:th "Latest Price"]]
      (for [commodity @commodities]
        (commodity-row commodity))]]))

(defn commodities-page []
  (with-layout
    [:section
       [:h1 "Commodities"]
       [commodity-list]]))

(secretary/defroute "/commodities" []
  (r/render commodities-page (app-element)))
