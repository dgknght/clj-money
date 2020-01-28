(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer [defroutes
                                    wrap-routes
                                    routes
                                    ANY]]
            [compojure.handler :refer [site]]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body
                                          wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.util.response :refer [redirect]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-time.format :refer [unparse-local formatters]]
            [clj-money.core]
            [clj-money.json]
            [clj-money.web.auth :as web-auth]
            [clj-money.middleware :refer [wrap-integer-id-params
                                          wrap-models
                                          wrap-exceptions]]
            [clj-money.api :refer [wrap-authentication]]
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

; make sure we can handle joda types in json serialization
(cheshire.generate/add-encoder
  org.joda.time.LocalDate
  (fn [local-date gen]
    (.writeString gen (unparse-local (formatters :date) local-date))))

(defmacro model-route
  [method path handler]
  `(~method ~path req# (-> ~handler
                           wrap-models
                           wrap-integer-id-params)))

(def ^:private id-pattern #"^[0-9]+$")

#_(defroutes all-routes
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
  ; use /accounts/:id instead of /accounts/:id/transactions or /accounts/:id/transaction-items
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
  (model-route POST "/api/entities" entities-api/create)
  (model-route PATCH "/api/entities/:id" entities-api/update)
  (model-route DELETE "/api/entities/:id" entities-api/delete)

  (model-route GET "/api/entities/:entity-id/commodities" commodities-api/index)
  (model-route GET "/api/commodities/:id" commodities-api/get-one)
  (model-route POST "/api/entities/:entity-id/commodities" commodities-api/create)
  (model-route PATCH "/api/commodities/:id" commodities-api/update)
  (model-route DELETE "/api/commodities/:id" commodities-api/delete)

  (model-route GET "/api/commodities/:commodity-id/prices" prices-api/index)
  (model-route GET "/api/commodities/:entity-id/prices" prices-api/index)

  (model-route GET "/api/entities/:entity-id/accounts" accounts-api/index)
  (model-route GET "/api/accounts/:id" accounts-api/get-one)
  (model-route POST "/api/entities/:entity-id/accounts" accounts-api/create)
  (model-route PATCH "/api/accounts/:id" accounts-api/update)
  (model-route DELETE "/api/accounts/:id" accounts-api/delete)

  (model-route POST "/api/entities/:entity-id/trades" trading-api/create)

  (model-route GET "/api/transactions/:entity-id" transactions-api/index)
  (model-route GET "/api/transactions/:transaction-date/:id" transactions-api/get-one)
  (model-route POST "/api/entities/:entity-id/transactions" transactions-api/create)
  (model-route PATCH "/api/transactions/:transaction-date/:id" transactions-api/update)
  (model-route DELETE "/api/transactions/:transaction-date/:id" transactions-api/delete)

  (model-route GET "/api/transaction-items" transaction-items-api/index)

  (model-route GET "/api/accounts/:account-id/lots" lots-api/index)

  ; Imports
  (model-route GET "/api/imports" imports-api/index)
  (model-route POST "/api/imports" imports-api/create)
  (model-route DELETE "/api/imports/:id" imports-api/delete)
  (model-route GET "/api/imports/:id" imports-api/show)
  (model-route PUT "/api/imports/:id/start" imports-api/start)

  (GET "/" req pages/home)
  (GET "/login" req pages/login)
  (GET "/signup" req users/new-user)
  (POST "/users" req users/create-user)
  (GET "/users/:token/password" req users/new-password)
  (POST "/users/:token/password" req users/set-password))

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
                     (log/debugf "unable to match API route for \"%s\"." (:uri req))
                     {:status 404
                      :body {:message "not found"}
                      :headers {"Content-Type" "application/json"}})))
      (wrap-routes wrap-integer-id-params)
      (wrap-json-body {:keywords? true :bigdecimals? true})
      wrap-authentication
      wrap-exceptions
      wrap-json-response))

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
      wrap-keyword-params
      wrap-multipart-params
      wrap-content-type
      wrap-params))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
