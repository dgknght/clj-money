(ns clj-money.web.apps
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clj-money.config :refer [env]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.web.auth.google :as google-auth]
            [clj-money.web.auth.github :as github-auth]
            [hiccup.page :refer [html5 include-js]]))

(defn- needs-setup? []
  (zero? (entities/count (util/entity-type {} :user))))

(def ^:private all-oauth-providers
  {:google (google-auth/oauth2-profile)
   :github (github-auth/oauth2-profile)})

(def ^:private allow-listed-oauth-providers
  (if-let [providers (env :oauth-providerrs)]
    (select-keys all-oauth-providers
                 providers)
    all-oauth-providers))

(defn- oauth-providers []
  (->> allow-listed-oauth-providers
       (filter second)
       (map first)
       set))

(defn- head [extra-config]
  [:head
   [:meta  {:charset "utf-8"}]
   [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta  {:name "viewport" :content "width=device-width, initial-scale=1"}]
   "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"

   [:meta  {:name "description" :content "Double-entry accounting system"}]
   [:meta  {:name "author" :content "Doug Knight"}]
   [:link  {:rel "icon" :href "images/logo.svg"}]
   [:title (env :application-name "clj-money?")]
   [:script {:type "application/edn" :id "app-config"}
    (pr-str extra-config)]

   (include-js "https://unpkg.com/@popperjs/core@2")
   (include-js "js/bootstrap.min.js")

   [:link {:rel "stylesheet" :href "/css/site.css"}]
   [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1/font/bootstrap-icons.min.css"}]])

(defn index
  [_req]
  {:status 200
   :body (html5
           [:html.h-100 {:lang "en"}
            (head (merge {:needs-setup? (needs-setup?)
                          :oauth-providers (oauth-providers)}))
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

(def ^:private server-prefixes
  ["/api/" "/oapi/" "/auth/" "/app/"])

(defn spa-fallback
  [{:keys [request-method uri] :as req}]
  (when (and (= :get request-method)
             (not-any? #(string/starts-with? uri %) server-prefixes))
    (index req)))

(def routes
  ["" {:get {:handler index
             :middleware [wrap-request-logging]}}])
