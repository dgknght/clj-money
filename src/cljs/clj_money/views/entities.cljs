(ns clj-money.views.entities
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [clj-money.bootstrap :as bs]
            [clj-money.util :as util]
            [clj-money.api.entities :as entities]
            [clj-money.notifications :as notify]
            [clj-money.state :as state :refer [app-state]]
            [clj-money.plain-forms :refer [text-field
                                           #_radio-buttons]]))

(defn- delete
  [entity page-state]
  (when (js/confirm (str "Are you sure you want to delete the entity \"" (:name entity) "\"?"))
    (swap! page-state assoc :busy? true)
    (entities/delete entity
                     (fn []
                       (swap! page-state dissoc :busy?)
                       (state/remove-entity entity))
                     notify/danger)))

(defn find-entity
  [id]
  (->> (:entities @state/app-state)
       (filter #(= id (:id %)))
       first))

(defn- relay-updated-entity
  "Accepts an entity and replaces the corresponding enty
  in state/entities"
  [entity]
  (swap! app-state update-in [:entities]
         #(map (fn [e]
                 (if (= (:id e) (:id entity))
                   entity
                   e))
               %)))

(defn- save-entity
  [page-state]
  (let [entity (get-in @page-state [:selected])]
    (entities/save entity
                   (fn [result]
                     (if (:id entity)
                       (relay-updated-entity result)
                       (state/add-entity result))
                     (swap! page-state dissoc :selected))
                   (notify/danger-fn "Unable to save the entity: %s"))))

(defn- entity-form
  [page-state]
  (let [entity (r/cursor page-state [:selected])]
    (fn []
      [:div.card
       [:div.card-header [:strong (str (if (:id @entity) "Edit" "New") " Entity")]]
       [:div.card-body
        [:form
         [text-field entity [:name] {:validate [:required]}]
         #_[radio-buttons [:settings :inventory-method] ["fifo" "lifo"]]]]
       [:div.card-footer
        [:button.btn.btn-primary {:on-click #(save-entity page-state)}
         (bs/icon-with-text :check "Save")]
        (util/space)
        [:button.btn.btn-danger {:on-click #(swap! page-state dissoc :selected)}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- entity-row
  [entity page-state busy?]
  ^{:key entity}
  [:tr
   [:td
    (:name entity)]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-info {:on-click (fn []
                                               (swap! page-state assoc :selected entity)
                                               (util/set-focus "name"))
                                   :disabled busy?
                                   :title "Click here to edit this entity."}
      (bs/icon :pencil)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete entity page-state)
                                     :disabled busy?
                                     :title "Click here to remove this entity."}
      (bs/icon :x-circle)]]]])

(defn- entity-table
  [page-state]
  (let [entities (r/cursor state/app-state [:entities])
        busy? (r/cursor page-state [:busy?])]
    (fn []
      [:section {:class (when @busy? "busy")}
       [:table.table.table-hover
        [:tbody
         [:tr
          [:th.col-sm-10 "Name"]
          [:th.col-sm-2 " "]]
         (doall (map #(entity-row % page-state @busy?)
                     @entities))]]])))

(defn- entities-page []
  (let [page-state (r/atom {})
        current-entity (r/cursor app-state [:current-entity])
        selected (r/cursor page-state [:selected])]
    (fn []
      [:div.row.mt-5
       [:div.col-md-6
        [:h1 "Entities"]
        [entity-table page-state]
        [:button.btn.btn-primary {:on-click (fn []
                                              (swap! page-state
                                                     assoc
                                                     :selected
                                                     {:entity-id (:id @current-entity)})
                                              (util/set-focus "name"))
                                  :disabled (boolean @selected)
                                  :title "Click here to create a new entity."}
         (bs/icon-with-text :plus "Add")]
        (util/space)
        [:button.btn.btn-light {:on-click #(secretary/dispatch! "/imports")
                                  :title "Click here to import an entity from another accounting system"}
         (bs/icon-with-text :file-arrow-up "Import")]]
       (when @selected
         [:div.col-md-6
          [entity-form page-state]])])))

(secretary/defroute "/entities" []
  (swap! app-state assoc :page #'entities-page))
