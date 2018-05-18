(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [compojure.core :refer [defroutes routes GET PUT POST DELETE ANY]]
            [compojure.handler :refer [site]]
            [compojure.route :as route]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body
                                          wrap-json-params
                                          wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.util.response :refer [redirect]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [cemerick.friend.credentials :as creds]
            [cheshire.core :as json]
            [clj-money.core]
            [clj-money.json]
            [clj-money.middleware :refer [wrap-integer-id-params
                                          wrap-models
                                          wrap-exception-handling]]
            [clj-money.api.imports :as imports-api]
            [clj-money.api.entities :as entities-api]
            [clj-money.web.pages :as pages]
            [clj-money.web.entities :as entities]
            [clj-money.web.images :as images]
            [clj-money.web.imports :as imports]
            [clj-money.web.accounts :as accounts]
            [clj-money.web.apps :as apps]
            [clj-money.web.budgets :as budgets]
            [clj-money.web.commodities :as commodities]
            [clj-money.web.prices :as prices]
            [clj-money.web.transactions :as transactions]
            [clj-money.web.attachments :as attachments]
            [clj-money.web.trading :as trading]
            [clj-money.web.lots :as lots]
            [clj-money.web.reconciliations :as reconciliations]
            [clj-money.web.reports :as reports]
            [clj-money.web.users :as users]
            [clj-money.web.grants :as grants]))

(defmacro model-route
  [method path handler]
  `(~method ~path req# (-> ~handler
                           wrap-models
                           wrap-integer-id-params)))

(defroutes all-routes
  ; Entities
  (model-route GET "/entities" entities/index)
  (model-route GET "/entities/new" entities/new-entity)
  (model-route POST "/entities" entities/create-entity)
  (model-route GET "/entities/:id/edit" entities/edit-entity)
  (model-route POST "/entities/:id" entities/update)
  (model-route POST "/entities/:id/delete" entities/delete)

  ; User grants
  (model-route GET "/entities/:entity-id/grants" grants/index)
  (model-route GET "/entities/:entity-id/grants/new" grants/new-grant)
  (model-route POST "/entities/:entity-id/grants" grants/create)
  (model-route GET "/grants/:id/edit" grants/edit)
  (model-route POST "/grants/:id" grants/update)
  (model-route POST "/grants/:id/delete" grants/delete)

  ; Accounts
  (model-route GET "/entities/:entity-id/accounts" accounts/index)
  (model-route GET "/entities/:entity-id/accounts/new" accounts/new-account)
  (model-route POST "/entities/:entity-id/accounts" accounts/create)
  (model-route GET "/accounts/:id" accounts/show)
  (model-route GET "/accounts/:id/edit" accounts/edit)
  (model-route POST "/accounts/:id" accounts/update)
  (model-route POST "/accounts/:id/delete" accounts/delete)

  ; Budgets
  (model-route GET "/entities/:entity-id/budgets" budgets/index)
  (model-route GET "/entities/:entity-id/budgets/new" budgets/new-budget)
  (model-route POST "/entities/:entity-id/budgets" budgets/create)
  (model-route GET "/budgets/:id/edit" budgets/edit)
  (model-route POST "/budgets/:id" budgets/update)
  (model-route POST "/budgets/:id/delete" budgets/delete)
  (model-route GET "/budgets/:id" budgets/show)

  ; Budget items
  (model-route GET "/budgets/:budget-id/items/new/:method" budgets/new-item)
  (model-route POST "/budgets/:budget-id/items" budgets/create-item)
  (model-route GET "/budget-items/:id/edit/:method" budgets/edit-item)
  (model-route POST "/budget-items/:id" budgets/update-item)
  (model-route POST "/budget-items/:id/delete" budgets/delete-item)

  ; Budget monitors
  (model-route GET "/entities/:entity-id/monitors" entities/monitors)
  (model-route POST "/entities/:entity-id/monitors" entities/create-monitor)
  (model-route POST "/entities/:entity-id/monitors/:account-id/delete" entities/delete-monitor)

  ; Commodities
  (model-route GET "/entities/:entity-id/commodities" commodities/index)
  (model-route GET "/entities/:entity-id/commodities/new" commodities/new-commodity)
  (model-route POST "/entities/:entity-id/commodities" commodities/create)
  (model-route GET "/commodities/:id/edit" commodities/edit)
  (model-route POST "/commodities/:id" commodities/update)
  (model-route POST "/commodities/:id/delete" commodities/delete)

  ; Prices
  (model-route GET "/commodities/:commodity-id/prices" prices/index)
  (model-route GET "/commodities/:commodity-id/prices/new" prices/new-price)
  (model-route POST "/commodities/:commodity-id/prices" prices/create)
  (model-route POST "/commodities/:commodity-id/prices/fetch" prices/fetch)
  (model-route POST "/entities/:entity-id/prices/fetch" prices/fetch-all)
  (model-route POST "/prices/:id/delete" prices/delete)

  ; Transactions
  (model-route GET "/entities/:entity-id/transactions" transactions/index)
  (model-route GET "/entities/:entity-id/transactions/new" transactions/new-transaction)
  (model-route POST "/entities/:entity-id/transactions" transactions/create)
  (model-route GET "/transactions/:transaction-date/:id/edit" transactions/edit)
  (model-route POST "/transactions/:transaction-date/:id" transactions/update)
  (model-route POST "/transactions/:transaction-date/:id/delete" transactions/delete)

  ; Attachments
  (model-route GET "/transactions/:transaction-id/attachments" attachments/index)
  (model-route GET "/transactions/:transaction-id/attachments" attachments/index)
  (model-route GET "/transactions/:transaction-id/attachments/new" attachments/new-attachment)
  (model-route POST "/transactions/:transaction-id/attachments" attachments/create)
  (model-route GET "/attachments/:id/edit" attachments/edit)
  (model-route POST "/attachments/:id" attachments/update)
  (model-route POST "/attachments/:id/delete" attachments/delete)

  ; Images
  (model-route GET "/images/:image-id" images/show)

  ; Trading
  (model-route GET "/accounts/:account-id/purchases/new" trading/new-purchase)
  (model-route POST "/accounts/:account-id/purchases" trading/purchase)
  (model-route GET "/accounts/:account-id/sales/new" trading/new-sale)
  (model-route POST "/accounts/:account-id/sales" trading/sell)
  (model-route POST "/transactions/:transaction-id/unbuy" trading/unbuy)

  ; Lots
  (model-route GET "/accounts/:account-id/lots" lots/index)

  ; Reconciliations
  (model-route GET "/accounts/:account-id/reconciliations/new" reconciliations/new-reconciliation)
  (model-route POST "/accounts/:account-id/reconciliations" reconciliations/create)
  (model-route GET "/reconciliations/:id" reconciliations/show)
  (model-route GET "/reconciliations/:id/edit" reconciliations/edit)
  (model-route POST "/reconciliations/:id" reconciliations/update)
  (model-route POST "/reconciliations/:id/delete" reconciliations/delete)

  ; Imports
  (GET "/imports/new" req imports/new-import)
  
  ; Reports
  (model-route GET "/entities/:entity-id/reports" reports/render)
  (model-route GET "/entities/:entity-id/reports/:type" reports/render)

  ; Single page apps
  (GET "/apps" req apps/index)
  (GET "/apps/:id" req apps/show)

  (model-route GET "/api/entities" entities-api/index)
  (model-route DELETE "/api/entities/:id" entities-api/delete)

  ; Imports
  (POST "/api/imports" req imports-api/create)
  (model-route GET "/api/imports/:id" imports-api/show)

  (GET "/" req pages/home)
  (GET "/login" req pages/login)
  (GET "/signup" req users/new-user)
  (POST "/users" req users/create-user)
  (GET "/users/:token/password" req users/new-password)
  (POST "/users/:token/password" req users/set-password))

(def ^:private accept-map
  {"application/json" :json})

(defmulti render-404
  (fn [req]
    (let [accept (get (:headers req) "accept")]
      (get accept-map accept :html))))

(defmethod render-404 :json
  [req]
  {:status 404
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string {:message "not found"})})

(defmethod render-404 :html
  [req]
  (route/not-found (slurp (io/resource "404.html"))))

(defn wrap-authenticate
  [handler]
  (friend/authenticate
    handler
    {:workflows [(workflows/interactive-form)]
     :credential-fn (partial clj-money.models.users/authenticate (env :db))
     :redirect-on-auth? false}))

(defn- api?
  [{uri :uri}]
  (when uri
    (re-find #"^/api" uri)))

(def ^:private open-route-rules
  ["/"
   [:get "/login"]
   [:get "/signup"]
   [:post "/users"]
   [:get "/users/:token/password"]
   [:post "/users/:token/password"]])

(defn- open?
  [{uri :uri method :request-method}]
  (some (fn [rule]
          (if (coll? rule)
            (and (= uri (second rule))
                 (= method (first rule)))
            (= uri rule)))
        open-route-rules))

(defn- apply-wrapper
  [handler wrapper]
  (let [[wrapper-fn
         description
         test-fn] (if (= 2 (count wrapper))
                    (conj wrapper (constantly true))
                    wrapper)]
    (fn [{:keys [request-method uri] :as request}]
      (if (test-fn request)
        (do
          (log/trace "apply middleware " description " to " request-method " " uri)
          ((wrapper-fn handler) request))
        (handler request)))))

(defn-  wrap-routes
  [handler & wrappers]
  (reduce apply-wrapper
          handler
          wrappers))

(def wrapped-routes
  (wrap-routes all-routes
               [wrap-multipart-params               "multipart params" (complement api?)]
               [wrap-keyword-params                 "keyword params"   (complement api?)]
               [wrap-json-params                    "json params"      api?]
               [wrap-json-response                  "json response"    api?]
               [wrap-keyword-params                 "keyword params"]
               [wrap-params                         "params"]
               [wrap-exception-handling             "excpetion handling"]
               [wrap-content-type                   "content-type"]
               [#(friend/wrap-authorize % #{:user}) "authorization"    (complement open?)]
               [wrap-authenticate                   "authentication"   (complement open?)]
               [#(wrap-resource % "public")         "public resources"]))

(defroutes app
  wrapped-routes
  (friend/logout (POST "/logout" [] (redirect "/")))
  (ANY "*" req
       (do
         (log/debug "unable to match route for " (:uri req))
         (render-404 req))))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
