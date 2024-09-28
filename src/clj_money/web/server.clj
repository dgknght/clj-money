(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [cheshire.generate]
            [reitit.core :as reitit]
            [reitit.ring :as ring]
            [reitit.exception :refer [format-exception]]
            [ring.middleware.defaults :refer [wrap-defaults
                                              site-defaults
                                              api-defaults]]
            [ring.util.response :as res]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body
                                          wrap-json-response]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [config.core :refer [env]]
            [dgknght.app-lib.authorization :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.core]
            [clj-money.json]
            [clj-money.web.auth :as web-auth]
            [clj-money.web.images :as images]
            [clj-money.middleware :refer [wrap-integer-id-params
                                          wrap-exceptions]]
            [clj-money.models :as models]
            [clj-money.api :refer [log-error]]
            [clj-money.api.users :as users-api]
            [clj-money.api.imports :as imports-api]
            [clj-money.api.entities :as entities-api]
            [clj-money.api.commodities :as commodities-api]
            [clj-money.api.prices :as prices-api]
            [clj-money.api.accounts :as accounts-api]
            [clj-money.api.budgets :as budgets-api]
            [clj-money.api.reports :as reports-api]
            [clj-money.api.trading :as trading-api]
            [clj-money.api.transactions :as transactions-api]
            [clj-money.api.transaction-items :as transaction-items-api]
            [clj-money.api.scheduled-transactions :as sched-trans-api]
            [clj-money.api.attachments :as att-api]
            [clj-money.api.reconciliations :as recs-api]
            [clj-money.api.lots :as lots-api]
            [clj-money.web.users :refer [find-user-by-auth-token]]
            [clj-money.web.apps :as apps]))

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

(defmethod handle-exception ::models/not-found
  [_data]
  (not-found))

(defn wrap-web-exceptions
  [handler]
  (fn [req]
    (try
     (handler req)
     (catch clojure.lang.ExceptionInfo e
       (handle-exception (ex-data e)))
     (catch Exception e
       (log-error e "unexpected error")
       (internal-error)))))

(defn- wrap-request-logging
  [handler]
  (fn [{:keys [request-method uri query-string] :as req}]
    (if query-string
      (log/infof "Request %s \"%s?%s\"" request-method uri query-string)
      (log/infof "Request %s \"%s\"" request-method uri))
    (let [res (handler req)]
      (log/infof "Response %s \"%s\" -> %s" request-method uri (:status res))
      res)))

(defn- wrap-merge-path-params
  [handler]
  (fn [{:keys [path-params] :as req}]
    (handler (update-in req [:params] merge path-params))))

(defn- wrap-site []
  (let [c-store (cookie-store)]
    [wrap-defaults (update-in site-defaults
                              [:session]
                              merge
                              {:store c-store
                               :cookie-attrs {:same-site :lax
                                              :http-only true}})]))

(def app
  (ring/ring-handler
    (ring/router ["/" {:middleware [wrap-request-logging]}
                  apps/routes
                  web-auth/routes
                  [(assoc-in
                     images/routes
                     [1 :middleware]
                     [(wrap-site)
                      wrap-merge-path-params
                      wrap-integer-id-params
                      [api/wrap-authentication
                       {:authenticate-fn find-user-by-auth-token}]])]
                  ["oapi/" {:middleware [[wrap-json-body {:keywords? true :bigdecimals? true}]
                                         wrap-json-response
                                         wrap-merge-path-params
                                         wrap-integer-id-params
                                         wrap-exceptions]}
                   users-api/unauthenticated-routes]
                  ["api/" {:middleware [[wrap-defaults (-> api-defaults
                                                           (assoc-in [:params :multipart] true)
                                                           (assoc-in [:security :anti-forgery] false))]
                                        wrap-json-response
                                        wrap-merge-path-params
                                        wrap-integer-id-params
                                        [api/wrap-authentication
                                         {:authenticate-fn find-user-by-auth-token}]
                                        wrap-exceptions
                                        [wrap-json-body {:keywords? true :bigdecimals? true}]]}
                   users-api/routes
                   entities-api/routes
                   commodities-api/routes
                   accounts-api/routes
                   transactions-api/routes
                   att-api/routes
                   budgets-api/routes
                   imports-api/routes
                   prices-api/routes
                   lots-api/routes
                   recs-api/routes
                   reports-api/routes
                   trading-api/routes
                   transaction-items-api/routes
                   sched-trans-api/routes]]
                 {:conflicts (fn [conflicts]
                               (log/warnf "The application has conflicting routes: %s" (format-exception :path-conflicts nil  conflicts)))})
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
