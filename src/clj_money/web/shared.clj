(ns clj-money.web.shared
  (:require [clojure.tools.logging :as log]
            [clojure.set :as set]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.string :as s]
            [cemerick.friend :as friend]
            [clj-money.models.users :as users])
  (:use clj-money.inflection))

(defn glyph-link
  "Renders a link with a glyphicon"
  ([glyph url] (glyph-link glyph url {}))
  ([glyph url options]
   [:a (merge options {:href url})
    [:span {:arial-hidden true
            :class (format "glyphicon glyphicon-%s" (name glyph))}]]))

(def glyph-button-sizes
  {:large       "lg"
   :small       "sm"
   :extra-small "xs"})

(defn- glyph-button-css
  [options]
  (cond-> #{"btn"}
    true (conj (str "btn-" (name (get options :level :default))))
    (:size options) (conj (str "btn-"
                               (get glyph-button-sizes (:size options))))))

(defn glyph-button
  "Renders a button with a glyphicon"
  [glyph url options]
  (glyph-link glyph url (-> options
                            (assoc :class
                                   (s/join " " (glyph-button-css options)))
                            (dissoc :size :level))))

; TODO Wrap this up in a sharable library
(defn bootstrap-nav
  "Renders the site navigation using the structure for twitter bootstrap"
  [items]
  [:nav.navbar.navbar-default
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
      (map (fn [{:keys [url caption method]}]
             [:li
              [:a {:href url :data-method method :rel (when method "nofollow")} caption]])
           items)]
     [:div.navbar-right
      [:ul.nav.navbar-nav.navbar-right
       (if-let [user (friend/current-authentication)]
         (html
           [:li
            [:a {:href "#"} (users/full-name user)]]
           [:li
            (glyph-link :log-out "/logout" {:title "Click here to sign out."
                                            :data-method :post})])
         (html
           [:li
            [:a {:href "/signup"} "Sign up"]]
           [:li
            (glyph-link :log-in "/login" {:title "Click here to sign in."})]))]]]]])

(defn primary-nav
  "Renders the site primary navigation"
  []
  (let [user (friend/current-authentication)
        items [{:url "/entities"     :caption "Entities"}
               {:url "/transactions" :caption "Transactions"}
               {:url "/commodities"  :caption "Commodities"}]]
    (bootstrap-nav items)))

(defn render-alerts
  "Renders notifications as HTML"
  [alerts]
  (map (fn [alert]
         (let [[css-class message] (if (string? alert)
                           [:info alert]
                           [(:type alert) (:message alert)])]
           [:div {:class (str "alert alert-" (name css-class))} message]))
       alerts))

(defn layout
  "Renders content inside a standard page template"
  [page-title options & content]
  (html5
    [:html {:lang "en"}
     [:head
      [:meta  {:charset "utf-8"}]
      [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
      [:meta  {:name "viewport" :content "width=device-width, initial-scale=1"}]
      "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"

      [:meta  {:name "description" :content "Double-entry account system"}]
      [:meta  {:name "author" :content "Doug Knight"}]
      [:link  {:rel "icon" :href "../../favicon.ico"}]
      [:title (str "clj-money - " (if-let [site-title (:site-title options)]
                                    site-title
                                    page-title))]

      "<!-- jQuery -->"
      [:script {:src "/js/jquery-3.1.0.min.js"}]
      [:script {:src "/js/jquery-startup.js"}]

      "<!-- Bootstrap core CSS -->"
      [:link {:rel "stylesheet" :href "/css/bootstrap.min.css"}]
      [:link {:rel "stylesheet" :href "/css/bootstrap-theme.min.css"}]
      [:script  {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"}]]
     [:body
      (primary-nav)
      [:div.container {:style "margin-top: 2em;"}
       (html
         [:h1#page-title page-title]
         (if-let [alerts (:alerts options)]
           (render-alerts alerts))
         content)]]]))

(defn- input-field
  "Renders a HTML input field"
  ([model attribute options]
   (let [{{errors attribute} :errors} model]
     [:div.form-group {:class (when errors "has-error")}
      [:label.control-label {:for attribute} (humanize attribute)]
      [:input.form-control (merge options {:id attribute
                                           :name attribute
                                           :value (get model attribute)})]
      (let [errors (if (seq? (-> model :errors attribute))
                     (-> model :errors attribute)
                     [(-> model :errors attribute)])]
        (map #(vector :span.help-block %) errors))])))

(defn text-input-field
  ([model attribute] (text-input-field model attribute {}))
  ([model attribute options]
   (input-field model attribute (merge options {:type :text}))))

(defn password-input-field
  ([model attribute] (password-input-field model attribute {}))
  ([model attribute options]
   (input-field model attribute (merge options {:type :password}))))
