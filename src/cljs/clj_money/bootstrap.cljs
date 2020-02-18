(ns clj-money.bootstrap
  (:require [reagent.core :as r]))

(defmulti ^:private nav-item :role)

(defmethod ^:private nav-item :separator
  [{:keys [id]}]
  ^{:key (str "separator-" id)}
  [:li.divider {:role "separator"}])

(defmethod ^:private nav-item :dropdown
  [{:keys [children id caption active? tool-tip]}]
  (if-not (seq children)
    (throw "A dropdown nav item must have children"))

  ^{:key (str "menu-item-" id)}
  [:li.dropdown {:role "presentation"}
   [:a.dropdown-toggle {:href "#"
                        :title tool-tip
                        :class [(when active? "active")]
                        :data-toggle "dropdown"
                        :role "button"
                        :aria-haspopup true}
    caption
    [:span.caret]]
   [:ul.dropdown-menu
    (for [child children]
      (nav-item child))]])

(defmethod ^:private nav-item :default
  [{:keys [id caption url tool-tip active? on-click]
    :or {url "#"}}]
  ^{:key (str "menu-item-" id)}
  [:li
   [:a {:href url
        :title tool-tip
        :class (when active? "active")
        :on-click on-click}
    caption]])

(defn navbar
  "Renders a bootstrap nav bar"
  [{:keys [title title-url items secondary-items] :or {title-url "/"}}]
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

     [:a.navbar-brand {:href title-url} title]] 

    [:div#navbar.collapse.navbar-collapse
     [:ul.nav.navbar-nav {}
      (for [item items]
        (nav-item item))]
     (when (seq secondary-items)
       [:div.navbar-right
        [:ul.nav.navbar-nav
         (for [item secondary-items]
           (nav-item item))]])]]])

(defn alert
  [{:keys [message severity]
    :or {severity :info}
    :as alert}
   remove-fn]
  ^{:key (str "alert-" (:id alert))}
  [:div {:class ["alert" (str "alert-" (name severity))]
         :role "alert"}
   [:button.close {:type :button
                   :aria-label "Close"
                   :on-click (fn [_] (remove-fn alert))}
    [:span.glyphicon.glyphicon-remove {:aria-hidden "true"}]]
   message])

(defn nav-tabs
  [items]
  [:ul.nav.nav-tabs
   (doall (for [{:keys [active?
                        disabled?
                        hidden?
                        on-click
                        caption
                        elem-key]} items]
            ^{:key elem-key}
            [:li {:role "presentation"
                  :class (cond-> []
                           active? (conj "active")
                           disabled? (conj "disabled")
                           hidden? (conj "hidden"))}
             [:a {:href "#"
                  :on-click on-click}
              caption]]))])

(defn- page-item
  [index state]
  ^{:key (str "page-item-" index)}
  [:li {:class (when (= index (get-in @state [:page-index]))
                 "active")}
   [:a {:href "#"
        :on-click #(swap! state assoc :page-index index)}
    (inc index)]])

(defn pagination
  "Creates navigation for paged data. Expects an derefable map with the following:
     :total      - the total number of items in the data set
     :page-index - the current page index (0-based)
     :page-size  - the number of items per page"
  [state]
  (let [total (r/cursor state [:total])
        page-size (r/cursor state [:page-size])]
    (fn []
      [:nav {:aria-label "Pagination"}
       [:ul.pagination
        (->> (range (Math/ceil (/ @total @page-size)))
             (map #(page-item % state))
             doall)]])))
