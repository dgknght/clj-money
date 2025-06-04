(ns clj-money.web.apps
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [config.core :refer [env]]
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
   [:title (env :application-name "clj-money?")]

   (include-js "https://unpkg.com/@popperjs/core@2")
   (include-js "js/bootstrap.min.js")

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
                           "/cljs-out/clj-money.js"))]])})

(defn- wrap-request-logging
  [handler]
  (fn [{:keys [request-method uri query-string] :as req}]
    (if query-string
      (log/infof "Request %s \"%s?%s\"" request-method uri query-string)
      (log/infof "Request %s \"%s\"" request-method uri))
    (let [res (handler req)]
      (log/infof "Response %s \"%s\" -> %s" request-method uri (:status res))
      res)))

(def routes
  ["" {:get {:handler index
             :middleware [wrap-request-logging]}}])
