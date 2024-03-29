(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer [defroutes
                                    wrap-routes
                                    routes
                                    GET
                                    ANY]]
            [cheshire.generate]
            [ring.util.response :refer [header] :as res]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body
                                          wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [co.deps.ring-etag-middleware :as etag]
            [environ.core :refer [env]]
            [dgknght.app-lib.authorization :as authorization]
            [dgknght.app-lib.api :as api]
            [clj-money.core]
            [clj-money.json]
            [clj-money.web.auth :as web-auth]
            [clj-money.web.images :as images]
            [clj-money.middleware :refer [wrap-integer-id-params
                                          wrap-exceptions
                                          wrap-collection-params]]
            [clj-money.models :as models]
            [clj-money.api :refer [find-user-by-auth-token
                                   log-error]]
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
            [clj-money.web.apps :as apps]))

(defroutes api-routes
  (-> (routes users-api/routes
              imports-api/routes
              entities-api/routes
              accounts-api/routes
              budgets-api/routes
              reports-api/routes
              transactions-api/routes
              transaction-items-api/routes
              sched-trans-api/routes
              att-api/routes
              recs-api/routes
              commodities-api/routes
              lots-api/routes
              prices-api/routes
              trading-api/routes
              (ANY "/api/*" req
                (do
                  (log/debugf "unable to match API route for %s \"%s\"." (:request-method req) (:uri req))
                  api/not-found)))
      (wrap-routes wrap-integer-id-params)
      (wrap-json-body {:keywords? true :bigdecimals? true})
      (api/wrap-authentication {:authenticate-fn find-user-by-auth-token})
      wrap-exceptions
      wrap-json-response))

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

(defroutes protected-web-routes
  (-> images/routes
      (wrap-routes wrap-integer-id-params)
      (api/wrap-authentication {:authenticate-fn find-user-by-auth-token})
      wrap-web-exceptions))

(defn- wrap-no-cache-header
  [handler]
  (fn [req]
    (header (handler req) "Cache-Control" "no-cache")))

(defn- wrap-request-logging
  [handler]
  (fn [{:keys [request-method uri query-string] :as req}]
    (if query-string
      (log/infof "Request %s: %s?%s" request-method uri query-string)
      (log/infof "Request %s: %s" request-method uri))
    (handler req)))

(defroutes app
  (-> (routes apps/routes
              web-auth/routes
              (wrap-routes users-api/unauthenticated-routes
                           #(-> %
                                (wrap-json-body {:keywords? true})
                                wrap-json-response))
              api-routes
              protected-web-routes
              (GET "*" _ (res/redirect "/"))
              (ANY "*" req
                   (do
                     (log/debugf "unable to match route for \"%s\"." (:uri req))
                     (not-found))))
      (wrap-resource "public")
      wrap-cookies
      wrap-keyword-params
      wrap-collection-params
      wrap-multipart-params
      wrap-content-type
      wrap-params
      wrap-no-cache-header
      etag/wrap-file-etag
      wrap-not-modified
      wrap-request-logging))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty #'app {:port port :join? false})
    (log/infof "Web server listening on port %s" port)))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
