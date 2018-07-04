(ns clj-money.layout
  (:require [clj-money.bootstrap :as bootstrap]
            [clj-money.state :as state]
            [clj-money.notifications :refer [notifications unnotify]]
            [clj-money.util :refer [path]]))

(defn- entity->nav-item
  [{:keys [id name] :as entity}]
  {:id id
   :caption name
   :on-click #(reset! state/current-entity entity)})

(defn nav [active-nav]
  (bootstrap/nav-bar
    {:title "clj-money"
     :title-url "/apps/default"
     :items [{:id :commodities
              :active? (= :commodities active-nav)
              :url "/commodities"
              :caption "Commodities"
              :tool-tip "Click here to manage commodities"}]
     :secondary-items [{:id :html
                        :caption "HTML"
                        :url (path :entities
                                   (:id @state/current-entity)
                                   :accounts)}
                       {:id :entities
                        :role :dropdown
                        :caption (:name @state/current-entity)
                        :children (concat (map entity->nav-item
                                               @state/entities)
                                          [{:role :separator
                                            :id "entity-separator"}
                                           {:id "manage-entities"
                                            :url "/entities"
                                            :caption "Manage Entities"
                                            :tool-tip "Click here to manage your entities."}])}]}))
(defn with-layout
  [body]
  [:div
    [nav]
    [bootstrap/alerts notifications unnotify]
    [:div.container
     body]])
