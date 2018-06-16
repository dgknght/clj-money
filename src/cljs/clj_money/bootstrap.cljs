(ns clj-money.bootstrap)

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
    :or {url "#"}
    :as item}]
  ^{:key (str "menu-item-" id)}
  [:li
   [:a {:href url
        :title tool-tip
        :class (when active? "active")
        :on-click on-click}
    caption]])

(defn nav-bar
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

(defn- alert
  [{:keys [message severity]
    :or {severity :info}
    :as alert}
   remove-fn]
  ^{:key message}
  [:div {:class ["alert" (str "alert-" (name severity))]
         :role "alert"}
   [:button.close {:type :button
                   :aria-label "Close"
                   :on-click remove-fn}
    [:span.glyphicon.glyphicon-remove {:aria-hidden "true"}]]
   message])

(defn alerts
  "Renders alerts. The argument should be a sequence of maps containing:
    :message The text to be displayed
    :severity One of success, info, warning, or danger (optional, defaults to info)"
  [alerts remove-fn]
  (when (seq alerts)
    [:div#notifications.row
     [:div.col-md-6.col-md-offset-3
      (doall (map #(alert % remove-fn) alerts))]]))
