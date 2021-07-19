(ns clj-money.web.apps
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes
                                    GET]]
            [hiccup.page :refer [html5 include-js]]))

(defn- head []
  [:head
   [:meta  {:charset "utf-8"}]
   [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta  {:name "viewport" :content "width=device-width, initial-scale=1"}]
   "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"

   [:meta  {:name "description" :content "Double-entry accounting system"}]
   [:meta  {:name "author" :content "Doug Knight"}]
   [:link  {:rel "icon" :href "../../favicon.ico"}]
   [:title "clj-money"]

   "<!-- jQuery -->"
   [:script {:src "/js/jquery-3.1.0.min.js"}]
   [:script {:src "/js/jquery-ui.min.js"}]
   [:script {:src "/js/bootstrap/bootstrap.min.js"}]

   "<!-- Bootstrap core CSS -->"
   [:link {:rel "stylesheet" :href "/css/bootstrap/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :href "/css/jquery-ui.min.css"}]
   [:link {:rel "stylesheet" :href "/css/jquery-ui.structure.min.css"}]
   [:link {:rel "stylesheet" :href "/css/jquery-ui.theme.min.css"}]
   [:link {:rel "stylesheet" :href "/css/clj-money.css"}]])

(defn index []
  (html5
    [:html {:lang "en"}
     (head)
     [:body
      [:div#app
       [:nav.navbar.navbar-expand-lg.navbar-light.bg-light
        [:div.container "clj-money"]]
       [:div.container.mt-5
        [:div.spinner-border {:role :status}
         [:span.sr-only
          "Loading..."]]]]
      (include-js "/js/app/main.js")]]))

(defroutes routes
  (GET "/" [] (index)))
