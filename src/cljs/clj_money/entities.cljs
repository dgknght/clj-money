(ns clj-money.entities
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.util :as util]
            [clj-money.api.entities :as entities]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.forms :refer [text-input
                                     radio-buttons
                                     required]]))

(defn- delete
  [entity]
  (when (js/confirm "Are you sure you want to delete this entity?")
    (entities/delete entity
                     (fn []
                       (swap! state/entities (fn [old-list]
                                               (remove
                                                 #(= (:id %)
                                                     (:id entity))
                                                 old-list))))
                     notify/danger)))

(def ^:private entity-form
  [:form
   (text-input :name :required)
   (radio-buttons :settings.inventory-method ["fifo" "lifo"])])

(defn- create-entity
  [entity]
  (entities/create entity
                   (fn [created]
                     (swap! state/entities #(conj % created))
                     (secretary/dispatch! "/entities"))
                   #(notify/danger %)))

(defn- new-entity []
  (let [entity (r/atom {})]
    (with-layout
      [:div.row
       [:div.col-md-6
        [:h1 "New Entity"]
        [bind-fields entity-form entity]
        (util/button "Save" #(create-entity @entity) {:class "btn btn-primary"
                                                      :icon :ok})
        (util/space)
        [:a.btn.btn-danger {:href "/entities"}
         [:span.glyphicon.glyphicon-ban-circle {:aria-hidden "true"}]
         (util/space) "Cancel"]]])))

(defn find-entity
  [id]
  (->> @state/entities
       (filter #(= id (:id %)))
       first))

(defn- relay-updated-entity
  "Accepts an entity and replaces the corresponding enty
  in state/entities"
  [entity]
  (swap! state/entities
         #(map (fn [e]
                 (if (= (:id e)  (:id entity))
                   entity
                   e))
               %)))

(defn- save-entity
  [entity]
  (entities/update entity
                   (fn [entity]
                     (relay-updated-entity entity)
                     (secretary/dispatch! "/entities"))
                   #(notify/danger %)))

(defn edit-entity
  [id]
  (let [entity (r/atom (-> id js/parseInt find-entity))]
    (with-layout
      [:div.row
       [:div.col-md-6
        [:h1 "Edit Entity"]
        [bind-fields entity-form entity]
        (util/button "Save" #(save-entity @entity) {:class "btn btn-primary"
                                                    :icon :ok})
        (util/space)
        (util/link-to "Cancel" "/entities" {:class "btn btn-danger"
                                            :icon :ban-circle})]])))

(defn- entity-row
  [entity]
  ^{:key entity}
  [:tr
   [:td
    (:name entity)]
   [:td
    [:div.btn-group
     [:a.btn.btn-xs.btn-info {:href (util/path :entities (:id entity) :edit)
                              :title "Click here to edit this entity."}
      [:span.glyphicon.glyphicon-pencil {:aria-hidden true}]]
     [:a.btn.btn-xs.btn-danger {:on-click #(delete entity)
                                :title "Click here to remove this entity."}
      [:span.glyphicon.glyphicon-remove {:aria-hidden true}]]]]])

(defn- entity-table
  []
  [:section
  [:table.table.table-striped.table-hover
   [:tbody
    [:tr
     [:th.col-sm-10 "Name"]
     [:th.col-sm-2 " "]]
    (for [entity @state/entities]
      (entity-row entity))]]])

(defn entities-page []
  (with-layout
    [:div.row
     [:div.col-md-6
      [:h1 "Entities"]
      [entity-table]
      (util/add-button "/entities/new")]]))

(secretary/defroute new-entity-path "/entities/new" []
  (r/render [new-entity] (app-element)))

(secretary/defroute entity-path "/entities/:id/edit" {id :id}
  (r/render [edit-entity id] (app-element)))

(secretary/defroute entities-path "/entities" []
  (r/render [entities-page] (app-element)))
