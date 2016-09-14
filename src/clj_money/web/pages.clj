(ns clj-money.web.pages
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn navigation
  "Renders the site navigation"
  [items]
  [:nav.navbar
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
      (map (fn [item]
             [:li
              [:a {:href (:url item)} (:caption item)]
              ])
           items)]]]])

(defn primary-nav
  "Renders the site primary navigation"
  []
  (navigation [{:url "/accounts"     :caption "Accounts"}
               {:url "/transactions" :caption "Transactions"}
               {:url "/commodities"  :caption "Commodities"}]))

(defn layout
  "Renders content in a standard page"
  [site-title page-title & content]
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
      [:title (str "clj-money - " site-title)]

      "<!-- jQuery -->"
      [:script {:src "http://code.jquery.com/jquery-2.1.4.min.js"}]

      "<!-- Bootstrap core CSS -->"
      [:link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"}]
      [:link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css"}]
      [:link {:rel "stylesheet" :href "/clj-money.css"}]
      [:script  {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"}]]
     [:body
      (primary-nav)
      [:div.container {:style "margin-top: 2em;"}
       (html
         [:h1#page-title page-title]
         content)]]]))

(defn home
  "Renders the home page"
  []
  (layout "Home",
          "Money"
          (html
            "Welcome to the accounting application"
            )))
