(ns clj-money.state
  (:require [reagent.core :as r]))

(defonce app-state (r/atom {:mounted? false}))
(def current-user (r/cursor app-state [:current-user]))
(def current-entity (r/cursor app-state [:current-entity]))

(defn- remove-entity-from-list
  [state entity]
  (update-in state [:entities] (fn [old-list]
                                 (remove
                                   #(= (:id %)
                                       (:id entity))
                                   old-list))))

(defn- reset-removed-current-entity
  [state entity]
  (if (= (:id entity) (get-in state [:current-entity :id]))
    (if-let [[entity] (get-in state [:entities])]
      (assoc state :current-entity entity)
      (dissoc state :current-entity))
    state))

(defn remove-entity
  [entity]
  (swap! app-state #(-> %
                        (remove-entity-from-list entity)
                        (reset-removed-current-entity entity))))

(defn add-entity
  [entity]
  (swap! app-state #(-> %
                        (update-in [:entities] conj entity)
                        (assoc :current-entity entity))))

(defn logout []
  (swap! app-state dissoc :auth-token :current-user :entities :current-entity))
