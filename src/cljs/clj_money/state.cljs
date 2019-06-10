(ns clj-money.state
  (:require [reagent.core :as r]))

(defonce app (r/atom {}))

(defonce entities (r/atom []))

(defonce current-entity (r/atom nil))

(defn remove-entity
  [entity]
  (swap! entities (fn [old-list]
                          (remove
                            #(= (:id %)
                                (:id entity))
                            old-list)))
  (when (= (:id entity) (:id @current-entity))
    (if (seq @entities)
      (reset! current-entity (first @entities))
      (reset! current-entity nil))))

(defn add-entity
  [entity]
  (swap! entities conj entity)
  (reset! current-entity entity))
