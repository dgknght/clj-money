(ns clj-money.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clj-money.data :as data]
            [clj-money.notifications :refer [notifications unnotify]]
            [clj-money.entities :as entities]))

(def current-entity (r/atom nil))

(def entities (r/atom []))

(defn- load-entities []
  (data/get-entities (fn [result]
                       (reset! current-entity (first result))
                       (reset! entities result))))

(defn- entity-nav-item
  [entity]
  ^{:key entity}
  [:li {:class (when (= (:id entity)
                        (:id @current-entity))
                 "active")}
   [:a {:href "#"
        :on-click #(reset! current-entity entity)}
    (:name entity)]])

(defn nav []
  [:nav.navbar.navbar-inverse
   [:div.container
    [:div.navbar-header
     [:button.navbar-toggle.collapsed {:type "button"
                                       :data-toggle "collapse"
                                       :data-target "#navbar"
                                       :aria-expanded "false"
                                       :aria-controls "navbar"} 
      [:span.sr-only "Toggle navigation"] 
      [:span.icon-bar] 
      [:span.icon-bar] 
      [:span.icon-bar]] 

     [:a.navbar-brand {:href "/"} "clj-money"]] 

    [:div#navbar.collapse.navbar-collapse
     [:ul.nav.navbar-nav
      [:li.dropdown {:role "presentation"}
       [:a.dropdown-toggle
        {:href "#"
         :data-toggle "dropdown"
         :role "button"
         :aria-haspopup true}
        (:name @current-entity)
        [:span.caret]]
       [:ul.dropdown-menu
        (doall
          (concat
            (map entity-nav-item @entities)
            [^{:key :entities-separator} [:li.divider {:role "separator"}]
             ^{:key :manage-entities} [:li
              [:a {:href "/entities"} "Manage Entities"]]]))]]]]]])

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
   [nav]
   [alerts]
   [:div.container
    (entities/management entities)]])

(def app-element
  (.getElementById js/document "app"))

(secretary/defroute root-path "/" []
  (r/render home-page app-element))

(secretary/defroute entities-path "/entities" []
  (r/render entities-page app-element))

(defn mount-root []
  (r/render home-page app-element))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (mount-root)
  (load-entities))

(init!)