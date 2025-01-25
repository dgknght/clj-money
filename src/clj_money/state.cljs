(ns clj-money.state
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.cookies :as cookies]
            [dgknght.app-lib.core :as lib]))

(defonce app-state (r/atom (merge {:mounted? false
                                   :bg-proc-count 0}
                                  (cookies/get :state))))

(add-watch app-state
           ::init
           (fn [_ _ _ state]
             (cookies/set! :state (select-keys state [:auth-token]))))

(def current-user (r/cursor app-state [:current-user]))
(def current-entity (r/cursor app-state [:current-entity]))
(def auth-token (r/cursor app-state [:auth-token]))
(def accounts (r/cursor app-state [:accounts]))
(def accounts-by-id (make-reaction #(when @accounts (lib/index-by :id @accounts))))
(def bg-proc-count (r/cursor app-state [:bg-proc-count]))
(def busy? (make-reaction #(not (zero? @bg-proc-count))))

(defn +busy []
  (swap! bg-proc-count inc))

(defn -busy []
  (swap! bg-proc-count dec))

(def -busy-x
  (map (fn [x]
         (swap! bg-proc-count dec)
         x)))

(defn- remove-entity-from-list
  [entity entities]
  (remove #(= (:id %)
              (:id entity))
          entities))

(defn- reset-removed-current-entity
  [state entity]
  (if (= (:id entity) (get-in state [:current-entity :id]))
    (if-let [[entity] (get-in state [:entities])]
      (assoc state :current-entity entity)
      (dissoc state :current-entity))
    state))

(defn set-entities
  [[entity :as entities]]
  (swap! app-state #(assoc (if entity
                             (assoc % :current-entity entity)
                             (dissoc % :current-entity))
                           :entities
                           entities)))

(defn remove-entity
  [entity]
  (swap! app-state #(-> %
                        (update-in [:entities] (partial remove-entity-from-list entity))
                        (reset-removed-current-entity entity))))

(defn add-entity
  [entity]
  (swap! app-state #(-> %
                        (update-in [:entities] conj entity)
                        (assoc :current-entity entity))))

(defn logout []
  (swap! app-state dissoc :auth-token :current-user :entities :current-entity))
