(ns clj-money.core
  (:require [reagent.core :as reagent]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]))

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
      [:li {:class "active"}
       [:a {:href "#"} "About"]]
      [:li
       [:a {:href "#contact"}
        "Contact"]]]]]])

(defn home-page []
  [:div
   [nav]
   [:div.container
    [:h1 "This Is ClojureScript"]]])

(secretary/defroute "/" []
  (.log js/console "exercise the root route"))

(defn mount-root []
  (reagent/render home-page (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (mount-root))

(init!)
