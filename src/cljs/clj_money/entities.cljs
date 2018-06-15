(ns clj-money.entities
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.util :as util]
            [clj-money.data :as data]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.forms :refer [text-input
                                     radio-buttons
                                     required]]))

(defn- delete
  [entity]
  (data/delete-entity entity
                      (fn []
                        (swap! state/entities (fn [old-list]
                                              (remove
                                                #(= (:id %)
                                                    (:id entity))
                                                old-list))))
                      notify/danger))

(def ^:private entity-form
  [:form
   (text-input :name :required)
   (radio-buttons :settings.inventory-method ["fifo" "lifo"])
   ])

(defn find-entity
  [id]
  (->> @state/entities
       (filter #(= id (:id %)))
       first))

(defn- relay-updated-entity
  "Accepts an entity and replaces the corresponding enty
  in state/entities"
  [entity]

  (.log js/console "relay update " (prn-str entity))

  (swap! state/entities
         #(map (fn [e]

                 (.log js/console "test against " (prn-str e))

                 (if (= (:id e)  (:id entity))
                   entity
                   e))
               %)))

(defn- save-entity
  [entity]
  (data/update-entity entity
                      (fn [entity]
                        (relay-updated-entity entity)
                        (secretary/dispatch! "/entities"))
                      #(notify/danger %)))

(defn- cancel-edit
  [id]
  (.log js/console "reload the entity with this id: " (prn-str id)))

(defn edit-entity
  [id]
  (let [entity (r/atom (-> id js/parseInt find-entity))]
    (with-layout
      [:div.row
       [:h1 "Edit Entity"]
       [:div.col-md-6
        [bind-fields entity-form entity]
        [:button.btn.btn-primary {:type :button
                                  :on-click #(save-entity @entity)}
         [:span.glyphicon.glyphicon-ok {:aria-hidden "true"}]
         (util/space) "Save"]
        (util/space)
        [:button.btn.btn-danger {:on-click #(cancel-edit (:id @entity))}
         [:span.glyphicon.glyphicon-ban-circle {:aria-hidden "true"}]
         (util/space) "Cancel"]]])))

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
  [:table.table.table-striped.table-hover
      [:tbody
       [:tr
        [:th.col-sm-10 "Name"]
        [:th.col-sm-2 " "]]
       (for [entity @state/entities]
         (entity-row entity))]])

(defn entities-page []
  (with-layout
    [:div.row
     [:div.col-md-6
      [:h1 "Entities"]
      [entity-table]]]))

(secretary/defroute entity-path "/entities/:id/edit" {id :id}
  (r/render [edit-entity id] (app-element)))

(secretary/defroute entities-path "/entities" []
  (r/render [entities-page] (app-element)))
