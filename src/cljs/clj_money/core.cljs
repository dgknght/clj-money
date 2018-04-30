(ns clj-money.core
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]))

(def current-entity (r/atom nil))

(def entities
  [{:id 1 :name "Personal"}
   {:id 2 :name "Business"}])

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
            (for [entity entities]
              ^{:key entity} [:li {:class (when (= (:id entity)
                                                   (:id @current-entity))
                                            "active")}
                              [:a {:href "#"
                                   :on-click (fn []
                                               (.log js/console "selected entity " (prn-str entity))
                                               (reset! current-entity entity))}
                               (:name entity)]])
            [^{:key :entities-separator} [:li.divider {:role "separator"}]
             ^{:key :manage-entities} [:li
              [:a {:href "#"} "Manage Entities"]]]))]]]]]])

(defn home-page []
  [:div
   [nav]
   [:div.container
    [:h1 "This Is ClojureScript"]]])

(secretary/defroute "/" []
  (.log js/console "exercise the root route"))

(defn mount-root []
  (r/render home-page (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (mount-root))

(init!)
