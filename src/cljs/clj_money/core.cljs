(ns clj-money.core
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]))

(def app-state (r/atom {}))

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
        (-> @app-state :current-entity :name)
        [:span.caret]]
       [:ul.dropdown-menu
        (concat
          (for [entity entities]
            ^{:key entity} [:li {:class (when (= (:id entity)
                                                 (:id (:current-entity @app-state)))
                                          "active")}
                            [:a {:href "#"
                                 :on-click (fn []
                                             (.log js/console "selected entity")
                                             (swap! app-state
                                                    #(assoc % :currency-entity entity)))}
                             (:name entity)]])
          [[:li.divider {:role "separator"}]
           [:li
            [:a {:href "#"} "Manage Entities"]]])]]]]]])

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

(swap! app-state #(assoc % :current-entity (first entities)))

(init!)
