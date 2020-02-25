(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer [defroutes
                                    wrap-routes
                                    routes
                                    ANY]]
            [cheshire.generate]
            [ring.util.response :refer [header]]
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
            [clj-money.x-platform.util :refer [serialize-date]]
            [clj-money.core]
            [clj-money.json]
            [clj-money.web.auth :as web-auth]
            [clj-money.middleware :refer [wrap-integer-id-params
                                          wrap-exceptions]]
            [clj-money.api :as api :refer [wrap-authentication]]
            [clj-money.api.users :as users-api]
            [clj-money.api.imports :as imports-api]
            [clj-money.api.entities :as entities-api]
            [clj-money.api.commodities :as commodities-api]
            [clj-money.api.prices :as prices-api]
            [clj-money.api.accounts :as accounts-api]
            [clj-money.api.trading :as trading-api]
            [clj-money.api.transactions :as transactions-api]
            [clj-money.api.transaction-items :as transaction-items-api]
            [clj-money.api.lots :as lots-api]
            [clj-money.web.apps :as apps]))

; make sure we can handle joda types in json serialization
(cheshire.generate/add-encoder
  org.joda.time.LocalDate
  (fn [local-date gen]
    (.writeString gen (serialize-date local-date))))

(defroutes api-routes
  (-> (routes users-api/routes
              imports-api/routes
              entities-api/routes
              accounts-api/routes
              transactions-api/routes
              transaction-items-api/routes
              commodities-api/routes
              lots-api/routes
              prices-api/routes
              trading-api/routes
              (ANY "/api/*" req
                   (do
                     (log/debugf "unable to match API route for %s \"%s\"." (:request-method req) (:uri req))
                     (api/not-found))))
      (wrap-routes wrap-integer-id-params)
      (wrap-json-body {:keywords? true :bigdecimals? true})
      wrap-authentication
      wrap-exceptions
      wrap-json-response))

(defn- wrap-no-cache-header
  [handler]
  (fn [req]
    (header (handler req) "Cache-Control" "no-cache")))

(defroutes app
  (-> (routes apps/routes
              web-auth/routes
              api-routes
              (ANY "*" req
                   (do
                     (log/debugf "unable to match route for \"%s\"." (:uri req))
                     {:status 404
                      :body (slurp "resources/404.html")
                      :headers {"Content-Type" "text/html"}})))
      (wrap-resource "public")
      wrap-cookies
      wrap-keyword-params
      wrap-multipart-params
      wrap-content-type
      wrap-params
      wrap-no-cache-header
      etag/wrap-file-etag
      wrap-not-modified))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty #'app {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
