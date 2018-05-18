(ns clj-money.entities
  (:require [reagent.core :as r]
            [clj-money.data :as data]
            [clj-money.notifications :as notify]))

(def editing-entity (r/atom nil))

(defn- edit
  [entity]
  (reset! editing-entity entity))

(defn- finish-edit
  []
  (notify/warning "Not implemented.")
  (reset! editing-entity nil))

(defn- cancel-edit
  []
  (reset! editing-entity nil))

(defn- delete
  [entity entities]
  (data/delete-entity entity
                      (fn []
                        (swap! entities (fn [old-list]
                                          (remove
                                            #(= (:id %)
                                                (:id entity))
                                            old-list))))
                      notify/danger))


(defn- entity-row
  [entity entities]
  ^{:key entity}
  [:tr
   [:td (:name entity)]
   [:td
    [:div.btn-group {:class (when @editing-entity "hidden")}
     [:button.btn.btn-xs.btn-info {:on-click #(edit entity)
                                   :title "Click here to edit this entity."}
      [:span.glyphicon.glyphicon-pencil {:arial-hidden true}]]
     [:button.btn.btn-xs.btn-danger {:on-click #(delete entity entities)
                                     :title "Click here to remove this entity."}
      [:span.glyphicon.glyphicon-remove {:arial-hidden true}]]]
    [:div.btn-group {:class (when (not= (:id @editing-entity) (:id entity)) "hidden")}
     [:button.btn.btn-xs.btn-success {:on-click finish-edit
                                      :title "Click here to save your changes to this entity"}
      [:span.glyphicon.glyphicon-ok {:arial-hidden true}]]
     [:button.btn.btn-xs.btn-danger {:on-click cancel-edit
                                     :title "Click here to cancel this edit."}
      [:span.glyphicon.glyphicon-ban-circle{:arial-hidden true}]]]]])

(defn- entity-table
  [entities]
  [:table.table.table-striped.table-hover
      [:tbody
       [:tr
        [:th.col-sm-10 "Name"]
        [:th.col-sm-2 " "]]
       (doall (map #(entity-row % entities) @entities))]])

; Expects an atom containing a list of entities
(defn management
  "Renders an entity management form"
  [entities]
  [:section
   [:h1 "Entities"]
   [:div.row
    [:div.col-md-6
     [entity-table entities]]]])
