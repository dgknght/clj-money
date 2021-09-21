(ns clj-money.views.entities
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :refer [text-field]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.busy :refer [busy +busy -busy]]
            [clj-money.views.util :refer [handle-error]]
            [clj-money.api.entities :as entities]
            [clj-money.state :as state :refer [app-state]]))

(defn- delete
  [entity page-state]
  (when (js/confirm (str "Are you sure you want to delete the entity \"" (:name entity) "\"?"))
    (+busy page-state)
    (entities/delete entity
                     (fn []
                       (-busy page-state)
                       (state/remove-entity entity))
                     (handle-error page-state "Unable to delete the entity: %s"))))

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
    (+busy page-state)
    (entities/save entity
                   (fn [result]
                     (if (:id entity)
                       (relay-updated-entity result)
                       (state/add-entity result))
                     (swap! page-state #(-> %
                                            -busy
                                            (dissoc :selected))))
                   (handle-error page-state "Unable to save the entity: %s"))))

(defn- entity-form
  [page-state]
  (let [entity (r/cursor page-state [:selected])
        busy? (busy page-state)]
    (fn []
      [:div.card
       [:div.card-header [:strong (str (if (:id @entity) "Edit" "New") " Entity")]]
       [:div.card-body
        [:form
         [text-field entity [:name] {:validate [:required]}]
         #_[radio-buttons [:settings :inventory-method] ["fifo" "lifo"]]]]
       [:div.card-footer
        [bs/busy-button {:html {:on-click #(save-entity page-state)
                                :title "Click here to save this entity."
                                :class "btn-primary"}
                         :icon :check
                         :caption "Save"
                         :busy? busy?}]

        [bs/busy-button {:html {:class "btn-secondary ms-2"
                                :title "Click here to cancel this operation."
                                :on-click #(swap! page-state dissoc :selected)}
                         :icon :x
                         :caption "Cancel"
                         :busy? busy?}]]])))

(defn- entity-row
  [entity page-state busy?]
  ^{:key entity}
  [:tr
   [:td (:name entity)]
   [:td.text-end
    [:div.btn-group
     [:button.btn.btn-sm.btn-light {:on-click (fn []
                                                (swap! page-state assoc :selected entity)
                                                (html/set-focus "name"))
                                    :disabled busy?
                                    :title "Click here to edit this entity."}
      (bs/icon :pencil {:size :small})]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete entity page-state)
                                     :disabled busy?
                                     :title "Click here to remove this entity."}
      (bs/icon :x-circle {:size :small})]]]])

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
          [:th.col-sm-2 (html/space)]]
         (doall (map #(entity-row % page-state @busy?)
                     @entities))]]])))

(defn- entities-page []
  (let [page-state (r/atom {})
        current-entity (r/cursor app-state [:current-entity])
        selected (r/cursor page-state [:selected])
        busy? (busy page-state)]
    (fn []
      [:<>
       [:h1.mt-3 "Entities"]
       [:div.row
        [:div.col-md-6
         [entity-table page-state]
         [bs/busy-button {:html {:class "btn-primary"
                                 :title "Click here to create a new entity."
                                 :on-click (fn []
                                             (swap! page-state
                                                    assoc
                                                    :selected
                                                    {:entity-id (:id @current-entity)})
                                             (html/set-focus "name"))}
                          :disabled selected
                          :busy? busy?
                          :icon :plus
                          :caption "Add"}]
         [bs/busy-button {:html {:class "btn-secondary ms-2"
                                 :title "Click here to import an entity from another accounting system."
                                 :on-click #(secretary/dispatch! "/imports")}
                          :icon :file-arrow-up
                          :busy? busy?
                          :caption "Import"}]]
        (when @selected
          [:div.col-md-6
           [entity-form page-state]])]])))

(secretary/defroute "/entities" []
  (swap! app-state assoc :page #'entities-page))
