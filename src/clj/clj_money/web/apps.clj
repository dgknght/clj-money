(ns clj-money.web.apps
  (:refer-clojure :exclude [update])
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.web.shared :refer [head]]))

(defn index
  [_]
  (redirect "/apps/default"))

(defn show
  [{params :params}]
  (html5
    [:html {:lang "en"}
     (head "clj-money" {})
     [:body
      [:h1 "This is a test"]]]))

#_([:html
  {:lang "en"}
  [:head
   [:meta  {:charset "utf-8"}]
   [:meta
    {:content "width=device-width, initial-scale=1, shrink-to-fit=no",
     :name "viewport"}]
   [:meta  {:content "", :name "description"}]
   [:meta  {:content "", :name "author"}]
   [:link  {:href "../../../../favicon.ico", :rel "icon"}]
   [:title "Starter Template for Bootstrap"]
   "<!-- Bootstrap core CSS -->"
   [:link
    {:rel "stylesheet",
     :href "../../../../dist/css/bootstrap.min.css"}]
   "<!-- Custom styles for this template -->"
   [:link  {:rel "stylesheet", :href "starter-template.css"}]]
  [:body
   [:nav.navbar.navbar-expand-md.navbar-dark.bg-dark.fixed-top
    [:a.navbar-brand  {:href "#"} "Navbar"]
    [:button.navbar-toggler
     {:aria-label "Toggle navigation",
      :aria-expanded "false",
      :aria-controls "navbarsExampleDefault",
      :data-target "#navbarsExampleDefault",
      :data-toggle "collapse",
      :type "button"}
     [:span.navbar-toggler-icon]]
    [:div#navbarsExampleDefault.collapse.navbar-collapse
     [:ul.navbar-nav.mr-auto
      [:li.nav-item.active
       [:a.nav-link  {:href "#"} "Home "  [:span.sr-only "(current)"]]]
      [:li.nav-item  [:a.nav-link  {:href "#"} "Link"]]
      [:li.nav-item  [:a.nav-link.disabled  {:href "#"} "Disabled"]]
      [:li.nav-item.dropdown
       [:a#dropdown01.nav-link.dropdown-toggle
        {:aria-expanded "false",
         :aria-haspopup "true",
         :data-toggle "dropdown",
         :href "http://example.com"}
        "Dropdown"]
       [:div.dropdown-menu
        {:aria-labelledby "dropdown01"}
        [:a.dropdown-item  {:href "#"} "Action"]
        [:a.dropdown-item  {:href "#"} "Another action"]
        [:a.dropdown-item  {:href "#"} "Something else here"]]]]
     [:form.form-inline.my-2.my-lg-0
      [:input.form-control.mr-sm-2
       {:aria-label "Search", :placeholder "Search", :type "text"}]
      [:button.btn.btn-outline-success.my-2.my-sm-0
       {:type "submit"}
       "Search"]]]]
   [:main.container
    {:role "main"}
    [:div.starter-template
     [:h1 "Bootstrap starter template"]
     [:p.lead
      "Use this document as a way to quickly start any new project."
      [:br]
      " All you get is this text and a mostly barebones HTML document."]]]
   "<!-- /.container -->"
   "<!-- Bootstrap core JavaScript\n    ================================================== -->"
   "<!-- Placed at the end of the document so the pages load faster -->"
   [:script
    {:crossorigin "anonymous",
     :integrity
     "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
     :src "https://code.jquery.com/jquery-3.3.1.slim.min.js"}]
   [:script
    "window.jQuery || document.write('<script src=\"../../../../assets/js/vendor/jquery-slim.min.js\"><\\/script>')"]
   [:script  {:src "../../../../assets/js/vendor/popper.min.js"}]
   [:script  {:src "../../../../dist/js/bootstrap.min.js"}]]])

