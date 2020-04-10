(ns clj-money.bootstrap
  (:require [reagent.core :as r]))

(defmulti ^:private nav-item :role)

(defmethod ^:private nav-item :separator
  [{:keys [id]}]
  ^{:key (str "separator-" id)}
  [:li.dropdown-divider {:role "separator"}])

(defmethod ^:private nav-item :dropdown
  [{:keys [children id caption active? tool-tip]}]
  (if-not (seq children)
    (throw "A dropdown nav item must have children"))

  ^{:key (str "menu-item-" id)}
  [:li.nav-item.dropdown
   [:a.nav-link.dropdown-toggle {:href "#"
                                 :title tool-tip
                                 :class [(when active? "active")]
                                 :data-toggle "dropdown"
                                 :role "button"
                                 :aria-expanded false
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
  [:li.nav-item {:class (when active? "active")}
   [:a.nav-link {:href url
                 :title tool-tip
                 :on-click on-click}
    caption]])

(defn navbar
  "Renders a bootstrap nav bar"
  [{:keys [title title-url items secondary-items] :or {title-url "/"}}]
  [:nav.navbar.navbar-expand-lg.navbar-light.bg-light
   [:div.container
    [:a.navbar-brand {:href title-url} title]
    [:button.navbar-toggler {:type :button
                             :data-toggle :collapse
                             :data-target "#primary-navbar"
                             :aria-controls "primary-navbar"
                             :aria-expanded false
                             :aria-label "Toggle navigation"}
     [:span.navbar-toggler-icon]]

    [:div#primary-navbar.collapse.navbar-collapse
     [:ul.navbar-nav.mr-auto {}
      (for [item items]
        (nav-item item))]
     (when (seq secondary-items)
       [:ul.nav.navbar-nav
        (for [item secondary-items]
          (nav-item item))])]]])

(def ^:private icons
  {:arrow-left-short [[:path {:d "M7.854 4.646a.5.5 0 010 .708L5.207 8l2.647 2.646a.5.5 0 01-.708.708l-3-3a.5.5 0 010-.708l3-3a.5.5 0 01.708 0z"}]
                      [:path {:d "M4.5 8a.5.5 0 01.5-.5h6.5a.5.5 0 010 1H5a.5.5 0 01-.5-.5z"}]]
   :arrow-repeat     [[:path {:d "M2.854 7.146a.5.5 0 00-.708 0l-2 2a.5.5 0 10.708.708L2.5 8.207l1.646 1.647a.5.5 0 00.708-.708l-2-2zm13-1a.5.5 0 00-.708 0L13.5 7.793l-1.646-1.647a.5.5 0 00-.708.708l2 2a.5.5 0 00.708 0l2-2a.5.5 0 000-.708z"}]
                      [:path {:d "M8 3a4.995 4.995 0 00-4.192 2.273.5.5 0 01-.837-.546A6 6 0 0114 8a.5.5 0 01-1.001 0 5 5 0 00-5-5zM2.5 7.5A.5.5 0 013 8a5 5 0 009.192 2.727.5.5 0 11.837.546A6 6 0 012 8a.5.5 0 01.501-.5z"}]]
   :arrows-collapse  [[:path {:d "M2 8a.5.5 0 01.5-.5h11a.5.5 0 010 1h-11A.5.5 0 012 8zm6-7a.5.5 0 01.5.5V6a.5.5 0 01-1 0V1.5A.5.5 0 018 1z"}]
                      [:path {:d "M10.354 3.646a.5.5 0 010 .708l-2 2a.5.5 0 01-.708 0l-2-2a.5.5 0 11.708-.708L8 5.293l1.646-1.647a.5.5 0 01.708 0zM8 15a.5.5 0 00.5-.5V10a.5.5 0 00-1 0v4.5a.5.5 0 00.5.5z"}]
                      [:path {:d "M10.354 12.354a.5.5 0 000-.708l-2-2a.5.5 0 00-.708 0l-2 2a.5.5 0 00.708.708L8 10.707l1.646 1.647a.5.5 0 00.708 0z"}]]
   :arrows-expand    [[:path {:d "M2 8a.5.5 0 01.5-.5h11a.5.5 0 010 1h-11A.5.5 0 012 8zm6-1.5a.5.5 0 00.5-.5V1.5a.5.5 0 00-1 0V6a.5.5 0 00.5.5z"}]
                      [:path {:d "M10.354 3.854a.5.5 0 000-.708l-2-2a.5.5 0 00-.708 0l-2 2a.5.5 0 10.708.708L8 2.207l1.646 1.647a.5.5 0 00.708 0zM8 9.5a.5.5 0 01.5.5v4.5a.5.5 0 01-1 0V10a.5.5 0 01.5-.5z"}]
                      [:path {:d "M10.354 12.146a.5.5 0 010 .708l-2 2a.5.5 0 01-.708 0l-2-2a.5.5 0 01.708-.708L8 13.793l1.646-1.647a.5.5 0 01.708 0z"}]]
   :check            [[:path {:d "M13.854 3.646a.5.5 0 010 .708l-7 7a.5.5 0 01-.708 0l-3.5-3.5a.5.5 0 11.708-.708L6.5 10.293l6.646-6.647a.5.5 0 01.708 0z"}]]
   :collection       [[:path {:d "M14.5 13.5h-13A.5.5 0 011 13V6a.5.5 0 01.5-.5h13a.5.5 0 01.5.5v7a.5.5 0 01-.5.5zm-13 1A1.5 1.5 0 010 13V6a1.5 1.5 0 011.5-1.5h13A1.5 1.5 0 0116 6v7a1.5 1.5 0 01-1.5 1.5h-13zM2 3a.5.5 0 00.5.5h11a.5.5 0 000-1h-11A.5.5 0 002 3zm2-2a.5.5 0 00.5.5h7a.5.5 0 000-1h-7A.5.5 0 004 1z"}]]
   :eye              [[:path {:d "M16 8s-3-5.5-8-5.5S0 8 0 8s3 5.5 8 5.5S16 8 16 8zM1.173 8a13.134 13.134 0 001.66 2.043C4.12 11.332 5.88 12.5 8 12.5c2.12 0 3.879-1.168 5.168-2.457A13.134 13.134 0 0014.828 8a13.133 13.133 0 00-1.66-2.043C11.879 4.668 10.119 3.5 8 3.5c-2.12 0-3.879 1.168-5.168 2.457A13.133 13.133 0 001.172 8z"}]
                      [:path {:d "M8 5.5a2.5 2.5 0 100 5 2.5 2.5 0 000-5zM4.5 8a3.5 3.5 0 117 0 3.5 3.5 0 01-7 0z"}]]
   :file-arrow-up    [[:path {:d "M4 1h8a2 2 0 012 2v10a2 2 0 01-2 2H4a2 2 0 01-2-2V3a2 2 0 012-2zm0 1a1 1 0 00-1 1v10a1 1 0 001 1h8a1 1 0 001-1V3a1 1 0 00-1-1H4z"}]
                      [:path {:d "M4.646 7.854a.5.5 0 00.708 0L8 5.207l2.646 2.647a.5.5 0 00.708-.708l-3-3a.5.5 0 00-.708 0l-3 3a.5.5 0 000 .708z"}]
                      [:path {:d "M8 12a.5.5 0 00.5-.5v-6a.5.5 0 00-1 0v6a.5.5 0 00.5.5z"}]]
   :pencil           [[:path {:d "M11.293 1.293a1 1 0 011.414 0l2 2a1 1 0 010 1.414l-9 9a1 1 0 01-.39.242l-3 1a1 1 0 01-1.266-1.265l1-3a1 1 0 01.242-.391l9-9zM12 2l2 2-9 9-3 1 1-3 9-9z"}]
                      [:path {:d "M12.146 6.354l-2.5-2.5.708-.708 2.5 2.5-.707.708zM3 10v.5a.5.5 0 00.5.5H4v.5a.5.5 0 00.5.5H5v.5a.5.5 0 00.5.5H6v-1.5a.5.5 0 00-.5-.5H5v-.5a.5.5 0 00-.5-.5H3z"}]]
   :play             [[:path {:d "M10.804 8L5 4.633v6.734L10.804 8zm.792-.696a.802.802 0 010 1.392l-6.363 3.692C4.713 12.69 4 12.345 4 11.692V4.308c0-.653.713-.998 1.233-.696l6.363 3.692z"}]]
   :plus             [[:path {:d "M8 3.5a.5.5 0 01.5.5v4a.5.5 0 01-.5.5H4a.5.5 0 010-1h3.5V4a.5.5 0 01.5-.5z"}]
                      [:path {:d "M7.5 8a.5.5 0 01.5-.5h4a.5.5 0 010 1H8.5V12a.5.5 0 01-1 0V8z"}]]
   :plus-circle      [[:path {:d "M8 3.5a.5.5 0 01.5.5v4a.5.5 0 01-.5.5H4a.5.5 0 010-1h3.5V4a.5.5 0 01.5-.5z"}]
                      [:path {:d "M7.5 8a.5.5 0 01.5-.5h4a.5.5 0 010 1H8.5V12a.5.5 0 01-1 0V8z"}]
                      [:path {:d "M8 15A7 7 0 108 1a7 7 0 000 14zm0 1A8 8 0 108 0a8 8 0 000 16z"}]]
   :stop             [[:path {:d "M3.5 5A1.5 1.5 0 015 3.5h6A1.5 1.5 0 0112.5 5v6a1.5 1.5 0 01-1.5 1.5H5A1.5 1.5 0 013.5 11V5zM5 4.5a.5.5 0 00-.5.5v6a.5.5 0 00.5.5h6a.5.5 0 00.5-.5V5a.5.5 0 00-.5-.5H5z"}]]
   :x                [[:path {:d "M11.854 4.146a.5.5 0 010 .708l-7 7a.5.5 0 01-.708-.708l7-7a.5.5 0 01.708 0z"}]
                      [:path {:d "M4.146 4.146a.5.5 0 000 .708l7 7a.5.5 0 00.708-.708l-7-7a.5.5 0 00-.708 0z"}]]
   :x-circle         [[:path {:d "M8 15A7 7 0 108 1a7 7 0 000 14zm0 1A8 8 0 108 0a8 8 0 000 16z"}]
                      [:path {:d "M11.854 4.146a.5.5 0 010 .708l-7 7a.5.5 0 01-.708-.708l7-7a.5.5 0 01.708 0z"}]
                      [:path {:d "M4.146 4.146a.5.5 0 000 .708l7 7a.5.5 0 00.708-.708l-7-7a.5.5 0 00-.708 0z"}]]})

(defn- get-icon-def
  [icon-key]
  (when-let [elements (get-in icons [icon-key])]
    (map (fn [e]
           (update-in e [1] #(merge {:fill-rule :evenodd
                                     :clip-rule :evenodd}
                                    %)))
         elements)))

(defn icon
  ([icon-key]
   (icon icon-key {}))
  ([icon-key options]
   (if-let [elements (get-icon-def icon-key)]
     (apply vector
            :svg.bi
            {:class (str "bi-" (name icon-key))
             :width "1em"
             :height "1em"
             :view-box "0 0 16 16"
             :fill "currentColor"
             :xmlns "http://www.w3.org/2000/svg"}
            elements)
     [:span.bg-danger options (str "Icon " (name icon-key) " not found")])))

(defn icon-with-text
  [icon-key text]
  [:span.d-flex.align-items-center
   (icon icon-key)
   [:span.ml-1
    text]])

(defn alert
  [{:keys [message severity]
    :or {severity :info}
    :as alert}
   remove-fn]
  ^{:key (str "alert-" (:id alert))}
  [:div.alert.alert-dismissible.fade.show {:class (str "alert-" (name severity))
         :role "alert"}
   [:button.btn-sm.close {:type :button
                          :aria-label "Close"
                          :on-click (fn [_] (remove-fn alert))}
    (icon :x-circle)]
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
            [:li.nav-item
             [:a.nav-link {:href "#"
                           :on-click on-click
                           :class (cond
                                    active? "active"
                                    disabled? "disabled"
                                    hidden? "d-none")}
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
