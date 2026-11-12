(ns clj-money.web.apps
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clj-money.config :refer [env]]
            [clj-money.entities :as entities]
            [clj-money.web.system :as system]
            [hiccup.page :refer [html5 include-js]]
            [ring.util.response :as res])
  (:import [java.security MessageDigest]
           java.util.HexFormat))

(defn- content-hash
  [resource]
  (with-open [in (io/input-stream resource)]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur))))
      (subs (.formatHex (HexFormat/of) (.digest digest)) 0 12))))

(def ^:private versioned-path
  (memoize
    (fn [path]
      (if-let [resource (io/resource (str "public" path))]
        (str path "?v=" (content-hash resource))
        path))))

(defn- asset-url
  "Returns the path to the static asset with a hash of its content in the
  query string, so that each build of the asset has its own URL and no cache
  (the CDN's, the browser's or the service worker's) can serve a stale copy
  of it. The hash is computed once per asset, as the assets don't change
  while the server runs, except in development, where figwheel rebuilds them
  and the path is returned as-is."
  [path]
  (if (env :dev?)
    path
    (versioned-path path)))

(defn- needs-setup? []
  (zero? (entities/count {} :entity-type :user)))

(defn- head [extra-config]
  [:head
   [:meta  {:charset "utf-8"}]
   [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta  {:name "viewport" :content "width=device-width, initial-scale=1, viewport-fit=cover"}]
   "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"

   [:meta  {:name "description" :content "Double-entry accounting system"}]
   [:meta  {:name "author" :content "Doug Knight"}]
   [:link  {:rel "icon" :href "images/logo.svg"}]
   [:link  {:rel "manifest" :href "/manifest.json"}]
   [:meta  {:name "theme-color" :content "#6f42c1"}]
   [:link  {:rel "apple-touch-icon" :href "/images/apple-touch-icon.png"}]
   [:title (env :application-name "clj-money?")]
   [:script {:type "application/edn" :id "app-config"}
    (pr-str extra-config)]

   (include-js "https://unpkg.com/@popperjs/core@2")
   (include-js (asset-url "/js/bootstrap.min.js"))

   [:link {:rel "stylesheet" :href (asset-url "/css/site.css")}]
   [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1/font/bootstrap-icons.min.css"}]])

(defn index
  [req]
  {:status 200
   :body (html5
           [:html.h-100 {:lang "en"}
            (head (merge {:needs-setup? (needs-setup?)
                          :oauth-providers (-> req
                                               (system/component :services)
                                               :oauth
                                               keys
                                               set)
                          :dev? (boolean (env :dev?))
                          :otel-endpoint (env :otel-browser-endpoint)}))
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
                           (asset-url "/cljs-out/clj-money.js")))]])})

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

(defn- asset-path?
  "Returns true if the last segment of the path has a file extension, as a
  static asset's does and a client route's doesn't. A missing asset must get
  a 404, not the app's HTML, which the service worker would otherwise cache
  in the asset's place."
  [uri]
  (boolean (re-find #"\.[^/]+$" uri)))

(defn spa-fallback
  [{:keys [request-method uri] :as req}]
  (when (and (= :get request-method)
             (not-any? #(string/starts-with? uri %) server-prefixes)
             (not (asset-path? uri)))
    (index req)))

(defn wrap-asset-caching
  "Sets the Cache-Control header on static assets. An asset requested with a
  version (see asset-url) never changes at that URL, so it can be cached
  indefinitely. The service worker must always be checked against the origin,
  or the CDN would keep serving the previous build's worker after a deploy."
  [handler]
  (fn [{:keys [uri query-params] :as req}]
    (when-let [response (handler req)]
      (cond
        (= "/service-worker.js" uri)
        (res/header response "Cache-Control" "no-cache")

        (and (#{200 304} (:status response))
             (get query-params "v"))
        (res/header response "Cache-Control" "public, max-age=31536000, immutable")

        :else
        response))))

(def routes
  ["" {:get {:handler index
             :middleware [wrap-request-logging]}}])
