(ns clj-money.web.apps
  (:refer-clojure :exclude [update])
  (:require [config.core :refer [env]]
            [hiccup.page :refer [html5 include-js]]))

(defn- head []
  [:head
   [:meta  {:charset "utf-8"}]
   [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta  {:name "viewport" :content "width=device-width, initial-scale=1"}]
   "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"

   [:meta  {:name "description" :content "Double-entry accounting system"}]
   [:meta  {:name "author" :content "Doug Knight"}]
   [:link  {:rel "icon" :href "images/logo.svg"}]
   [:title "clj-money"]

   [:script {:src "/js/bootstrap.min.js"}]

   [:link {:rel "stylesheet" :href "/css/site.css"}]])

(defn index
  [_req]
  {:status 200
   :body (html5
           [:html.h-100 {:lang "en"
                         :data-bs-theme "dark"}
            (head)
            [:body.h-100
             [:div#app.h-100
              [:nav.navbar.navbar-expand-lg.bg-body-tertiary
               [:div.container
                [:img {:src "/images/logo.svg"
                       :alt "abacus logo"
                       :width 24
                       :height 24}]]]
              [:div.container.mt-3
               [:div.d-flex.justify-content-around
                [:div.spinner-border {:role :status}
                 [:span.visually-hidden
                  "Loading..."]]]]]
             (include-js (if (env :dev?)
                           "/cljs-out/dev-main.js"
                           "/js/app/main.js"))]])})

(def routes
  ["" {:get {:handler index}}])
