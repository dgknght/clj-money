(ns clj-money.entities
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [clj-money.util :as util]
            [clj-money.data :as data]
            [clj-money.notifications :as notify]
            [clj-money.forms :refer [text-input
                                     select-input
                                     required]]))

(def all-entities (r/atom []))

(def current (r/atom nil))

(def ^:private editing-entity
  (r/atom nil))

(defn- edit
  [entity]
  (reset! editing-entity entity))

(defn- finish-edit
  []
  ; TODO Add something to show the user the save is happening in the background
  (let [updated (data/update-entity @editing-entity (constantly true) #(notify/danger %))]
    (swap! all-entities #(map (fn [e]
                               (if (= (:id e)  (:id updated))
                                 updated
                                 e)))))
  (reset! editing-entity nil))

(defn- cancel-edit
  []
  (reset! editing-entity nil))

(defn- delete
  [entity]
  (data/delete-entity entity
                      (fn []
                        (swap! all-entities (fn [old-list]
                                              (remove
                                                #(= (:id %)
                                                    (:id entity))
                                                old-list))))
                      notify/danger))

(def ^:private entity-form
  [:form
   (text-input :name required)
   (select-input :settings.inventory-method ["fifo" "lifo"])
   [:button.btn.btn-primary {:type :button :on-click finish-edit}
    [:span.glyphicon.glyphicon-ok {:aria-hidden "true"}]
    (util/space) "Save"]
   (util/space)
   [:button.btn.btn-danger {:type :button :on-click cancel-edit}
    [:span.glyphicon.glyphicon-ban-circle {:aria-hidden "true"}]
    (util/space) "Cancel"]])

(defn- entity-row
  [entity]
  ^{:key entity}
  [:tr
   [:td
    (:name entity)]
   [:td
    [:div.btn-group
     [:button.btn.btn-xs.btn-info {:on-click #(edit entity)
                                   :title "Click here to edit this entity."}
      [:span.glyphicon.glyphicon-pencil {:arial-hidden true}]]
     [:button.btn.btn-xs.btn-danger {:on-click #(delete entity)
                                     :title "Click here to remove this entity."}
      [:span.glyphicon.glyphicon-remove {:arial-hidden true}]]]]])

(defn- entity-table
  []
  [:table.table.table-striped.table-hover
      [:tbody
       [:tr
        [:th.col-sm-10 "Name"]
        [:th.col-sm-2 " "]]
       (for [entity @all-entities]
         (entity-row entity))]])

(defn management
  "Renders an entity management form"
  []
  [:section
   [:div.row {:class (when-not @editing-entity "hidden")}
    [:h1 "Edit Entity"]
    [:div.col-md-6
     [bind-fields entity-form editing-entity]]]
   [:div.row {:class (when @editing-entity "hidden")}
    [:div.col-md-6
     [:h1 "Entities"]
     [entity-table]]]])

(defn load
  "Loads the entities that are available to the user"
  []
  (data/get-entities (fn [result]
                       (swap! current (fnil identity (first result)))
                       (reset! all-entities result))))

(defn active?
  "Returns a boolean value indicating whether or not
  the specified entity is the active entity"
  [entity]
  (= (:id entity)
     (:id @current)))

(defn activate
  "Sets the specified entity as the active entity"
  [entity]
  (reset! current entity))
