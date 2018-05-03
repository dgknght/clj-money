(ns clj-money.entities
  (:require [clj-money.data :as data]))

(defn- edit
  [entity entities]
  (.log js/console "edit " (prn-str entity)))

(defn- delete
  [entity entities]
  (data/delete-entity entity (fn []
                               (swap! entities (fn [old-list]
                                                 (remove 
                                                   #(= (:id %)
                                                       (:id entity))
                                                   old-list))))))


(defn- entity-row
  [entity entities]
  ^{:key entity}
  [:tr
   [:td (:name entity)]
   [:td
    [:div.button-grp
     [:button.btn.btn-xs.btn-info {:on-click #(edit entity entities)
                                   :title "Click here to edit this entity."}
      [:span.glyphicon.glyphicon-pencil {:arial-hidden true}]]
     [:button.btn.btn-xs.btn-danger {:on-click #(delete entity entities)
                                     :title "Click here to remove this entity."}
      [:span.glyphicon.glyphicon-remove {:arial-hidden true}]]]]])

; Expects an atom containing a list of entities
(defn management
  "Renders an entity management form"
  [entities]
  [:section
   [:h1 "Entities"]
   [:div.row
    [:div.col-md-6
     [:table.table.table-striped.table-hover
      [:tbody
       [:tr
        [:th.col-sm-10 "Name"]
        [:th.col-sm-2 " "]]
       (doall (map #(entity-row % @entities) @entities))]]]]])
