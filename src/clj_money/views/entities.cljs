(ns clj-money.views.entities
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [secretary.core :as secretary :include-macros true]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :refer [text-field
                                           select-field]]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.util :refer [model=]]
            [clj-money.components :refer [button]]
            [clj-money.icons :refer  [icon]]
            [clj-money.api.entities :as entities]
            [clj-money.api.commodities :as commodities]
            [clj-money.state :as state :refer [app-state
                                               +busy
                                               -busy]]))

(defn- delete
  [entity]
  (when (js/confirm (str "Are you sure you want to delete the entity \"" (:name entity) "\"?"))
    (+busy)
    (entities/delete entity
                     (map (fn []
                            (-busy)
                            (state/remove-entity entity))))))

(defn- relay-updated-entity*
  [entity]
  (fn [state]
    (-> state
        (update-in [:entities]
                   #(map (fn [e]
                           (if (model= e entity)
                             entity
                             e))
                         %))
        (update-in [:currenty-entity]
                   (fn [e]
                     (if (model= e entity)
                       entity
                       e))))))

(defn- relay-updated-entity
  "Accepts an entity and replaces the corresponding enty
  in state/entities"
  [entity]
  (swap! app-state (relay-updated-entity* entity)))

(defn- save-entity
  [page-state]
  (let [entity (get-in @page-state [:selected])]
    (+busy)
    (entities/save entity
                   :callback -busy
                   :on-success (fn [result]
                                 (if (:id entity)
                                   (relay-updated-entity result)
                                   (state/add-entity result))
                                 (swap! page-state dissoc :selected)))))

(defn- entity-form
  [page-state]
  (let [entity (r/cursor page-state [:selected])
        commodities (r/cursor page-state [:commodities])
        comm-opts (make-reaction
                    (fn []
                      (->> @commodities
                           (filter #(= :currency (:commodity/type %)))
                           (mapv (juxt :id :commodity/name)))))]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (v/validate entity)
                           (when (v/valid? entity)
                             (save-entity page-state)))}
       [:div.card
        [:div.card-header [:strong (str (if (:id @entity) "Edit" "New") " Entity")]]
        [:div.card-body
         [text-field entity [:entity/name] {:validations #{::v/required}}]
         [select-field
          entity
          [:entity/settings
           :settings/default-commodity-id]
          comm-opts]
         #_[radio-buttons [:entity/settings :settings/inventory-method] ["fifo" "lifo"]]]
        [:div.card-footer
         [button {:html {:title "Click here to save this entity."
                         :class "btn btn-primary"
                         :type :submit}
                  :icon :check
                  :caption "Save"}]
         [button {:html {:type :button
                         :class "ms-2 btn-secondary"
                         :title "Click here to cancel this operation."
                         :on-click #(swap! page-state dissoc :selected)}
                  :caption "Cancel"
                  :icon :x}]]]])))

(defn- load-commodities
  [page-state]
  (if-let [selected (:selected @page-state)]
    (do (+busy)
        (commodities/select
          {:commodity/entity selected}
          :callback -busy
          :on-success (fn [c]
                        (swap! page-state
                               assoc
                               :commodities
                               (sort-by :commodity/name c)))))
    (swap! page-state dissoc :commodities)))

(defn- entity-row
  [entity page-state busy?]
  ^{:key (str "entity-row-" (:id entity))}
  [:tr
   [:td (:entity/name entity)]
   [:td.text-end
    [:div.btn-group
     [:button.btn.btn-sm.btn-secondary {:on-click (fn []
                                                    (swap! page-state assoc :selected entity)
                                                    (load-commodities page-state)
                                                    (set-focus "name"))
                                        :disabled busy?
                                        :title "Click here to edit this entity."}
      (icon :pencil :size :small)]
     [:button.btn.btn-sm.btn-danger {:on-click #(delete entity)
                                     :disabled busy?
                                     :title "Click here to remove this entity."}
      (icon :x-circle :size :small)]]]])

(defn- entities-table
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

(defn- index []
  (let [page-state (r/atom {})
        selected (r/cursor page-state [:selected])]
    (fn []
      [:<>
       [:h1.mt-3 "Entities"]
       [:div.row
        [:div.col-md-6 {:class (when @selected "d-none d-md-block")}
         [entities-table page-state]
         [button
          {:html {:title "Click here to create a new entity."
                  :class "btn-primary"
                  :on-click (fn []
                              (swap! page-state
                                     assoc
                                     :selected
                                     {})
                              (set-focus "name"))}
           :caption "Add"
           :icon :plus}]
         [button
          {:html {:title "Click here to import an entity from another accounting system."
                  :class "ms-2 btn-secondary"
                  :on-click #(secretary/dispatch! "/imports")}
           :caption "Import"
           :icon :upload}]]
        (when @selected
          [:div.col-md-6
           [entity-form page-state]])]])))

(secretary/defroute "/entities" []
  (swap! app-state assoc :page #'index))
