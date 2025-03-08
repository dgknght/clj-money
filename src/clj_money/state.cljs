(ns clj-money.state
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.cookies :as cookies]
            [dgknght.app-lib.core :as lib]
            [clj-money.util :as util]))

(defn- serialize
  "Minimize the data in the state cookie and also avoid writing
  anything that requires a custom tag reader (like dates) to avoid
  race conditions with the tag reader registration"
  [state]
  (-> state
      (lib/update-in-if [:current-user] util/->model-ref)
      (lib/update-in-if [:current-entity] util/->model-ref)
      (select-keys [:auth-token
                    :current-user
                    :current-entity])))

(defonce app-state (r/atom (merge {:mounted? false
                                   :bg-proc-count 0}
                                  (cookies/get :state))))

(add-watch app-state
           ::init
           (fn [_ _ _ state]
             (cookies/set! :state (serialize state))))

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
  (swap! bg-proc-count (lib/fmin dec 0)))

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
  (let [current (if-let [c @current-entity]
                  (->> entities
                       (filter #(util/model= c %))
                       first)
                  entity)]
    (swap! app-state assoc
           :entities entities
           :current-entity current)))

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
