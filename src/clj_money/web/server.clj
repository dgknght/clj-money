(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [cheshire.generate]
            [reitit.core :as reitit]
            [reitit.ring :as ring]
            [reitit.exception :refer [format-exception]]
            [reitit.middleware :as middleware]
            [ring.middleware.defaults :refer [wrap-defaults
                                              site-defaults
                                              api-defaults]]
            [ring.util.response :as res]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [dgknght.app-lib.authorization :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.otel.web :as otel]
            [clj-money.config :refer [env]]
            [clj-money.core]
            [clj-money.json]
            [clj-money.decimal :as d]
            [clj-money.web.auth :as web-auth]
            [clj-money.web.images :as images]
            [clj-money.middleware :refer [wrap-parse-id-params
                                          wrap-exceptions
                                          wrap-format]]
            [clj-money.entities :as entities]
            [clj-money.db.ref]
            [clj-money.api.users :as users-api]
            [clj-money.api.imports :as imports-api]
            [clj-money.api.entities :as entities-api]
            [clj-money.api.commodities :as commodities-api]
            [clj-money.api.prices :as prices-api]
            [clj-money.api.accounts :as accounts-api]
            [clj-money.api.budgets :as budgets-api]
            [clj-money.api.budget-items :as budget-items-api]
            [clj-money.api.reports :as reports-api]
            [clj-money.api.trading :as trading-api]
            [clj-money.api.transactions :as transactions-api]
            [clj-money.api.transaction-items :as transaction-items-api]
            [clj-money.api.scheduled-transactions :as sched-trans-api]
            [clj-money.api.attachments :as att-api]
            [clj-money.api.reconciliations :as recs-api]
            [clj-money.api.lots :as lots-api]
            [clj-money.api.cli-auth :as cli-auth-api]
            [clj-money.web.users :refer [find-user-by-auth-token]]
            [clj-money.web.apps :as apps]
            [clj-money.web.cli-auth :as cli-auth]
            [cljs.pprint :as pprint]))

(defn- not-found []
  (-> (slurp "resources/404.html")
      res/response
      (res/status 404)
      (res/content-type "text/html")))

(defn- forbidden []
  (-> (slurp "resources/403.html")
      res/response
      (res/status 403)
      (res/content-type "text/html")))

(defn internal-error []
  (-> (slurp "resources/500.html")
      res/response
      (res/status 500)
      (res/content-type "text/html")))

; TODO: Remove this duplicate with the API version of this fn
(defmulti handle-exception :type)

(defmethod handle-exception ::authorization/unauthorized
  [data]
  (if (:opaque? data)
    (not-found)
    (forbidden)))

(defmethod handle-exception ::authorization/not-found
  [_data]
  (not-found))

(defmethod handle-exception ::authorization/no-rules
  [_data]
  (internal-error))

(defmethod handle-exception ::entities/not-found
  [_data]
  (not-found))

(defn- wrap-request-logging
  [handler]
  (fn [{:keys [request-method uri query-string] :as req}]
    (if query-string
      (log/infof "Request %s \"%s?%s\"" request-method uri query-string)
      (log/infof "Request %s \"%s\"" request-method uri))
    (log/debugf "Request details %s \"%s\": %s"
                request-method
                uri
                (with-out-str
                  (pprint
                    (dissoc req
                            :reitit.core/match
                            :reitit.core/router))))
    (let [res (handler req)]
      (log/infof "Response %s \"%s\" -> %s" request-method uri (:status res))
      (log/debugf "Response details %s \"%s\": %s"
                  request-method
                  uri
                  (with-out-str
                    (pprint res)))
      res)))

(defn- wrap-merge-params
  [handler]
  (fn [{:keys [path-params body-params] :as req}]
    (handler (update-in req [:params] merge path-params body-params))))

(defn- wrap-site []
  (let [c-store (cookie-store)]
    [wrap-defaults (update-in site-defaults
                              [:session]
                              merge
                              {:store c-store
                               :cookie-attrs {:same-site :lax
                                              :http-only true}})]))

(defn- wrap-decimals
  [handler]
  (fn [req]
    (-> req
        handler
        (update-in [:body] d/wrap-decimals))))


(def app
  (ring/ring-handler
    (ring/router ["/" {:middleware [otel/wrap-otel]}
                  apps/routes
                  ["auth/" {:middleware [:site
                                         wrap-merge-params
                                         wrap-request-logging]}
                   web-auth/routes]
                  ["cli/" {:middleware [:site
                                        wrap-merge-params
                                        wrap-request-logging]}
                   cli-auth/routes]
                  ["app/" {:middleware [:site
                                        :wrap-format
                                        wrap-merge-params
                                        wrap-parse-id-params
                                        :authentication
                                        wrap-request-logging]}
                   images/routes]
                  ["oapi/" {:middleware [:api
                                         :wrap-format
                                         wrap-decimals
                                         wrap-merge-params
                                         wrap-parse-id-params
                                         wrap-exceptions
                                         wrap-request-logging]}
                   users-api/unauthenticated-routes
                   cli-auth-api/unauthenticated-routes]
                  ["api/" {:middleware [:api
                                        :wrap-format
                                        wrap-decimals
                                        wrap-merge-params
                                        wrap-parse-id-params
                                        :authentication
                                        wrap-exceptions
                                        wrap-request-logging]}
                   users-api/routes
                   entities-api/routes
                   commodities-api/routes
                   accounts-api/routes
                   transactions-api/routes
                   att-api/routes
                   budgets-api/routes
                   budget-items-api/routes
                   imports-api/routes
                   prices-api/routes
                   lots-api/routes
                   recs-api/routes
                   reports-api/routes
                   trading-api/routes
                   transaction-items-api/routes
                   sched-trans-api/routes
                   cli-auth-api/authenticated-routes]]
                 {:conflicts (fn [conflicts]
                               (log/warnf "The application has conflicting routes: %s" (format-exception :path-conflicts nil  conflicts)))
                  ::middleware/registry {:site (wrap-site)
                                         :api [wrap-defaults (-> api-defaults
                                                                 (assoc-in [:params :multipart] true)
                                                                 (assoc-in [:security :anti-forgery] false))]
                                         :wrap-format wrap-format
                                         :authentication [api/wrap-authentication
                                                          {:authenticate-fn find-user-by-auth-token}]}})
    (ring/routes
      (ring/create-resource-handler {:path "/"})
      (ring/create-default-handler))))

(defn print-routes []
  (pprint
    (map (comp #(take 2 %)
               #(update-in % [1] dissoc :middleware))
         (-> app
             ring/get-router
             reitit/compiled-routes))))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 3000))]
    (println (format "Starting web server on port %s..." port))
    (log/infof "Starting web server on port %s" port)
    (let [server (jetty/run-jetty #'app {:port port :join? false})]
      (log/infof "Web server listening on port %s" port)
      (println (format "Web server listening on port %s." port))
      server)))
