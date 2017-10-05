(ns clj-money.web.shared
  (:require [clojure.tools.logging :as log]
            [clojure.set :as set]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.anti-forgery :refer [anti-forgery-field]]
            [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
            [clojure.string :as s]
            [cemerick.friend :as friend]
            [clj-money.util :refer [format-number
                                    format-date]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities])
  (:use clj-money.inflection))

(defn append-anti-forgery-link-attributes
  [attributes]
  (if (= :post (:data-method attributes))
    (assoc attributes :data-anti-forgery-token *anti-forgery-token*)
    attributes))

(defn glyph-link
  "Renders a link with a glyphicon"
  ([glyph url] (glyph-link glyph url {}))
  ([glyph url options]
   [:a (-> options
           (merge {:href url})
           append-anti-forgery-link-attributes)
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
   {:url "/entities/:entity-id/budgets"      :caption "Budgets"}
   {:url "/entities/:entity-id/transactions" :caption "Transactions"}
   {:url "/entities/:entity-id/commodities"  :caption "Commodities"}
   {:url "/entities/:entity-id/reports"      :caption "Reports"}])

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

(defn- tabbed-nav-item
  [item active?]
  [:li {:role :presentation
        :class (when active? :active)}
   [:a {:href (:url item)}
    (:caption item)]])

(defn tabbed-nav
  "Renders tabbed navigation. Expects a sequence of maps like:
    :caption - the text to be displayed on the tab
    :url     - The URL to which the navigation tab links"
  [items current]
  [:ul.nav.nav-tabs
   (map #(tabbed-nav-item % (= current (:id %))) items)])

(defn render-alerts
  "Renders notifications as HTML"
  [alerts]
  (map (fn [alert]
         (let [[css-class message] (if (string? alert)
                           [:info alert]
                           [(:type alert) (:message alert)])]
           [:div {:class (str "alert alert-" (name css-class))} message]))
       alerts))

(defn head
  [page-title options]
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
   [:script {:src "/js/jquery-ui.min.js"}]
   [:script {:src "/js/jquery-startup.js"}]
   [:script {:src "/js/bootstrap.min.js"}]
   [:script {:src "/js/paced-progress-bar.js"}]

   "<!-- Bootstrap core CSS -->"
   [:link {:rel "stylesheet" :href "/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/css/bootstrap-theme.min.css"}]
   [:link {:rel "stylesheet" :href "/css/jquery-ui.min.css"}]
   [:link {:rel "stylesheet" :href "/css/jquery-ui.structure.min.css"}]
   [:link {:rel "stylesheet" :href "/css/jquery-ui.theme.min.css"}]
   [:link {:rel "stylesheet" :href "/css/clj-money.css"}]
   ])

(defn get-entity-from-options
  [options]
  (or (:entity options)
      (when (:entity-id options)
        (entities/find-by-id (env :db) (:entity-id options)))))

(defmacro with-layout
  "Renders content inside a standard page template"
  [page-title options & content]
  `(html5
    [:html {:lang "en"}
     (head ~page-title ~options)
     [:body
      (primary-nav (get-entity-from-options ~options))
      [:div.container {:style "margin-top: 2em;"}
       (html
         (let [side-bar# (:side-bar ~options)]
           [:div.row
            [:div {:class (format "col-md-%s" (if side-bar# 8 12))}
             (when-not (:suppress-page-title? ~options)
               [:h1#page-title ~page-title])
             (if-let [alerts# (:alerts ~options)]
               (render-alerts alerts#))
             ~@content]
            (when side-bar#
              [:div#side-bar.col-md-4 side-bar#])]))]]]))

(defn- input-element
  [name value options]
  [:div.form-group
    (when-not (:suppress-label? options)
      [:label.control-label {:for name} (humanize name)])
    [:input.form-control (-> options
                             (dissoc :format-fn)
                             (merge {:id name
                                     :name name
                                     :value value}))]])

(defn text-input-element
  ([name value] (text-input-element name value {}))
  ([name value options]
   (input-element name value (merge options {:type :text}))))

(defn hidden-input-element
  [name value]
  [:input {:type :hidden :name name :value value}])

(defmacro ^:private form-group
  [model attribute suppress-label? & body]
  `(vector :div.form-group {:class (when (validation/has-error? ~model ~attribute)
                                     "has-error")}
           (when-not ~suppress-label?
             [:label.control-label {:for ~attribute} (humanize ~attribute)])
           ~@body 
           (when (validation/has-error? ~model)
             (map #(vector :span.help-block %) (validation/error-messages ~model ~attribute)))))

(defn- input-field
  "Renders a HTML input field"
  ([model attribute options]
   (form-group model attribute (:suppress-label? options)
               [:input.form-control (-> options
                                        (dissoc :format-fn)
                                        (merge {:id attribute
                                                :name attribute
                                                :value ((or (:format-fn options)
                                                            identity) (get model attribute))}))])))

(defn date-input-field
  ([model attribute] (date-input-field model attribute {}))
  ([model attribute options]
   (form-group model attribute (:suppress-label? options)
               [:div.input-group.date-field
                [:input.form-control
                 (merge options
                        {:id attribute
                         :name attribute
                         :value (format-date (get model attribute))})]
                [:span.input-group-btn {:title "Click here to select a date."}
                 [:button.btn.btn-default
                  [:span.glyphicon.glyphicon-calendar {:aria-hidden true}]]]])))

(defn text-input-field
  ([model attribute] (text-input-field model attribute {}))
  ([model attribute options]
   (input-field model attribute (merge options {:type :text}))))

(defn number-input-field
  ([model attribute] (number-input-field model attribute {}))
  ([model attribute options]
   (input-field model attribute (merge {:format-fn #(format-number % {:format :no-comma})
                                        :step "0.01"}
                                       options
                                       {:type :number}))))

(defn password-input-field
  ([model attribute] (password-input-field model attribute {}))
  ([model attribute options]
   (input-field model attribute (merge options {:type :password}))))

(defn file-input-field
  ([model attribute] (file-input-field model attribute {}))
  ([model attribute options]
   (input-field model attribute (merge options {:type :file}))))

(defn select-element ; TODO probably need a better name here
  [name value option-items options]
  [:div.form-group
    (when-not (:suppress-label? options)
      [:label {:for name} (humanize name)])
    [:select.form-control (merge (dissoc options :suppress-label?) {:id name :name name})
     (map #(if (= value (get-in % [1 :value]))
             (assoc-in % [1 :selected] true)
             %)
          option-items)]])

(defn select-field
  ([model attribute option-items]
   (select-field model attribute option-items {}))
  ([model attribute option-items options]
   (select-element attribute (get model attribute) option-items options)))

(defn options-for-select
  [models caption-fn value-fn]
  (map #(vector :option
                {:value (value-fn %)}
                (caption-fn %))
       models))

(defmacro form
  [url options & body]
  `[:form {:action ~url
           :method (or (:method ~options) :post)}
    (anti-forgery-field)
    ~@body])
