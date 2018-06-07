(ns clj-money.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clj-money.util :refer [path]]
            [clj-money.data :as data]
            [clj-money.notifications :refer [notifications unnotify]]
            [clj-money.entities :as entities]
            [clj-money.commodities :as commodities]
            [clj-money.bootstrap :as bootstrap]))

(defn- entity->nav-item
  [{:keys [id name] :as entity}]
  {:id id
   :caption name
   :on-click #(entities/activate entity)})

(defn nav [active-nav]
  (bootstrap/nav-bar
    {:title "clj-money"
     :title-url "/"
     :items [{:id :commodities
              :active? (= :commodities active-nav)
              :url "/commodities"
              :caption "Commodities"
              :tool-tip "Click here to manage commodities"}]
     :secondary-items [{:id :html
                        :caption "HTML"
                        :url (path :entities
                                   (:id @entities/current)
                                   :accounts)}
                       {:id :entities
                        :role :dropdown
                        :caption (:name @entities/current)
                        :children (concat (map entity->nav-item
                                               @entities/all-entities)
                                          [{:role :separator
                                            :id "entity-separator"}
                                           {:id "manage-entities"
                                            :url "/entities"
                                            :caption "Manage Entities"
                                            :tool-tip "Click here to manage your entities."}])}]}))

(defn alert
  [index {:keys [message severity] :as notification}]
  ^{:key notification}
  [:div {:class ["alert" (str "alert-" (name severity))]
         :role "alert"}
   [:button.close {:type :button
                   :aria-label "Close"
                   :on-click #(unnotify index)}
    [:span.glyphicon.glyphicon-remove {:aria-hidden "true"}]]
   message])

(defn alerts []
  (when (seq @notifications)
    [:div#notifications.row
     [:div.col-md-6.col-md-offset-3
      (doall (map-indexed alert @notifications))]]))

(defn home-page []
  [:div
   [nav]
   [alerts]
   [:div.container
    [:h1 "This Is ClojureScript"]]])

(defn entities-page []
  [:div
   [nav :entities]
   [alerts]
   [:div.container
    [entities/management]]])

(defn commodities-page []
  [:div
   [nav :commodities]
   [alerts]
   [:div.container
    [commodities/management]]])

(defn app-element []
  (.getElementById js/document "app"))

(secretary/defroute "/" []
  (r/render home-page (app-element)))

(secretary/defroute "/entities" []
  (r/render entities-page (app-element)))

(secretary/defroute "/commodities" []
  (r/render commodities-page (app-element)))

(defn mount-root []
  (r/render home-page (app-element)))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (mount-root)
  (entities/load))

(init!)
