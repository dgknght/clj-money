(ns clj-money.web.shared
  (:require [clojure.tools.logging :as log]
            [clojure.set :as set]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.string :as s]
            [cemerick.friend :as friend]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities])
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

(defn- authenticated-user-nav
  "Renders the top level navigation for the authenticated user"
  [user entity]
  (html
    [:li
     [:a {:href "#"} (users/full-name user)]]
    [:li.dropdown {:role "presentation"}
     [:a.dropdown-toggle {:href (str "/entities/" (:id entity) "/accounts")
                          :data-toggle "dropdown"
                          :role "button"
                          :aria-haspopup "true"
                          :aria-expanded "false"}
      (:name entity)
      [:span.caret]]
     [:ul.dropdown-menu
      (let [other-entities (remove #(= (:id %) (:id entity)) (entities/select (env :db) (:id user)))]
        (when (seq other-entities)
          (concat
            (map #(vector :li
                          [:a {:href (str "/entities/" (:id %) "/accounts")}
                           (:name %)])
                 other-entities)
            [[:li.divider {:role "separator"}]])))
      [:li
       [:a {:href "/entities"} "Manage entities"]]]]
    [:li
     (glyph-link :log-out "/logout" {:title "Click here to sign out."
                                     :data-method :post})]))

(defn- unauthenticated-nav
  "Renders the top level navigation for a non-authenticated user"
  []
  (html
    [:li
     [:a {:href "/signup"} "Sign up"]]
    [:li
     (glyph-link :log-in "/login" {:title "Click here to sign in."})]))

; TODO Wrap this up in a sharable library
(defn- bootstrap-nav
  "Renders the site navigation using the structure for twitter bootstrap"
  [items user entity]
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
       (if user
         (authenticated-user-nav user entity)
         (unauthenticated-nav))]]]]])

(def item-templates
  [{:url "/entities/:entity-id/accounts"     :caption "Accounts"}
   {:url "/entities/:entity-id/transactions" :caption "Transactions"}
   {:url "/entities/:entity-idcommodities"  :caption "Commodities"}])

(defn primary-nav
  "Renders the site primary navigation"
  [entity]
  (if-let [user (friend/current-authentication)]
    (if-let [entity (or entity (first (entities/select (env :db) (:id user))))]
      (let [items (->> item-templates
                       (map (fn [item]
                              (update-in item
                                         [:url]
                                         #(string/replace % ":entity-id" (str (:id entity)))))))]
        (bootstrap-nav items user entity))
      (bootstrap-nav [] user nil))
    (bootstrap-nav [] nil nil)))

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
      [:script {:src "/js/bootstrap.min.js"}]

      "<!-- Bootstrap core CSS -->"
      [:link {:rel "stylesheet" :href "/css/bootstrap.min.css"}]
      [:link {:rel "stylesheet" :href "/css/bootstrap-theme.min.css"}]]
     [:body
      (primary-nav (:entity options))
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
