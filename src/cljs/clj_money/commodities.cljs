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

(defn- delete-commodity
  [commodity]
  (js/alert "not implemented yet."))

(defn- commodity-row
  [commodity]
  ^{:key (:id commodity)}
  [:tr
   [:td (:name commodity)]
   [:td (:symbol commodity)]
   [:td (:exchange commodity)]
   [:td "coming soon..."]
   [:td
    [:div.btn-group
     (util/link-to nil
                   (util/path :commodities (:id commodity) :edit)
                   {:icon :pencil
                    :class "btn btn-info btn-xs"
                    :title "Click here to edit this commodity."})
     (util/button nil
                  #(delete-commodity commodity)
                  {:title "Click here to remove this commodity."
                   :icon :remove
                   :class "btn btn-danger btn-xs"})]]])

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
         [:th "Latest Price"]
         [:td (util/space)]]
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
  (data/create-commodity commodity
                         #(secretary/dispatch! "/commodities")
                         notify/danger))

(def ^:private commodity-form
  [:form
   (select-input :type ["currency" "stock"])
   (select-input :exchange ["nyse" "nasdaq"] {:visible? #(= (:type %) "stock")})
   (text-input :symbol :required)
   (text-input :name :required)])

(defn- new-commodity []
  (let [commodity (r/atom {:entity-id (:id @state/current-entity)})]
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

(defn- edit-commodity
  [id]
  (with-layout
    (let [commodity (r/atom {})]
      (data/get-commodity id #(reset! commodity %))
      [:div.row
       [:div.col-md-6
        [:h1 "Edit Commodity"]
        [bind-fields commodity-form commodity]
        (util/button "Save"
                     #(data/update-commodity @commodity
                                             (fn [_]
                                               (secretary/dispatch! "/commodities"))
                                             notify/danger)
                     {:class "btn btn-primary"
                      :title "Click here to save this commodity"
                      :icon :ok})
        (util/space)
        (util/link-to "Cancel"
                      "/commodities"
                      {:class "btn btn-danger"
                       :title "Click here to return to the list of commodities."
                       :icon :ban-circle})]])))

(secretary/defroute "/commodities/new" []
  (r/render [new-commodity] (app-element)))

(secretary/defroute "/commodities" []
  (r/render [commodities-page] (app-element)))

(secretary/defroute "/commodities/:id/edit" [id]
  (r/render [edit-commodity id] (app-element)))
