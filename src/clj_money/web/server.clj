(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [compojure.core :refer [defroutes GET PUT POST DELETE ANY]]
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
            [ring.util.response :refer [redirect]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [cemerick.friend.credentials :as creds]
            [clj-money.json]
            [clj-money.middleware :refer [wrap-integer-id-params
                                          wrap-entity
                                          wrap-exception-handling]]
            [clj-money.web.pages :as pages]
            [clj-money.web.entities :as entities]
            [clj-money.web.images :as images]
            [clj-money.web.imports :as imports]
            [clj-money.api.imports :as imports-api]
            [clj-money.web.accounts :as accounts]
            [clj-money.web.budgets :as budgets]
            [clj-money.web.commodities :as commodities]
            [clj-money.web.prices :as prices]
            [clj-money.web.transactions :as transactions]
            [clj-money.web.attachments :as attachments]
            [clj-money.web.trading :as trading]
            [clj-money.web.lots :as lots]
            [clj-money.web.reconciliations :as reconciliations]
            [clj-money.web.reports :as reports]
            [clj-money.web.users :as users]))

(defmacro route
  [method path handler]
  `(~method ~path req# (-> ~handler
                           wrap-entity
                           wrap-integer-id-params)))

(defroutes protected-routes
  ; Entities
  (route GET "/entities" entities/index)
  (route GET "/entities/new" entities/new-entity)
  (route POST "/entities" entities/create-entity)
  (route GET "/entities/:id/edit" entities/edit-entity)
  (route POST "/entities/:id" entities/update)
  (route POST "/entities/:id/delete" entities/delete)

  ; Accounts
  (route GET "/entities/:entity-id/accounts" accounts/index)
  (route GET "/entities/:entity-id/accounts/new" accounts/new-account)
  (route POST "/entities/:entity-id/accounts" accounts/create)
  (route GET "/accounts/:id" accounts/show)
  (route GET "/accounts/:id/edit" accounts/edit)
  (route POST "/accounts/:id" accounts/update)
  (route POST "/accounts/:id/delete" accounts/delete)

  ; Budgets
  (route GET "/entities/:entity-id/budgets" budgets/index)
  (route GET "/entities/:entity-id/budgets/new" budgets/new-budget)
  (route POST "/entities/:entity-id/budgets" budgets/create)
  (route GET "/budgets/:id/edit" budgets/edit)
  (route POST "/budgets/:id" budgets/update)
  (route POST "/budgets/:id/delete" budgets/delete)
  (route GET "/budgets/:id" budgets/show)

  ; Budget items
  (route GET "/budgets/:budget-id/items/new/:method" budgets/new-item)
  (route POST "/budgets/:budget-id/items" budgets/create-item)
  (route GET "/budget-items/:id/edit/:method" budgets/edit-item)
  (route POST "/budget-items/:id" budgets/update-item)

  ; Budget monitors
  (route GET "/entities/:entity-id/monitors" entities/monitors)
  (route POST "/entities/:entity-id/monitors" entities/create-monitor)
  (route POST "/entities/:entity-id/monitors/:account-id/delete" entities/delete-monitor)

  ; Commodities
  (route GET "/entities/:entity-id/commodities" commodities/index)
  (route GET "/entities/:entity-id/commodities/new" commodities/new-commodity)
  (route POST "/entities/:entity-id/commodities" commodities/create)
  (route GET "/commodities/:id" commodities/show)
  (route GET "/commodities/:id/edit" commodities/edit)
  (route POST "/commodities/:id" commodities/update)
  (route POST "/commodities/:id/delete" commodities/delete)

  ; Prices
  (route GET "/commodities/:commodity-id/prices" prices/index)
  (route GET "/commodities/:commodity-id/prices/new" prices/new-price)
  (route POST "/commodities/:commodity-id/prices" prices/create)
  (route POST "/commodities/:commodity-id/prices/fetch" prices/fetch)
  (route POST "/entities/:entity-id/prices/fetch" prices/fetch-all)
  (route POST "/prices/:id/delete" prices/delete)

  ; Transactions
  (route GET "/entities/:entity-id/transactions" transactions/index)
  (route GET "/entities/:entity-id/transactions/new" transactions/new-transaction)
  (route POST "/entities/:entity-id/transactions" transactions/create)
  (route GET "/transactions/:id/edit" transactions/edit)
  (route POST "/transactions/:id" transactions/update)
  (route POST "/transactions/:id/delete" transactions/delete)

  ; Attachments
  (route GET "/transactions/:transaction-id/attachments" attachments/index)
  (route GET "/transactions/:transaction-id/attachments" attachments/index)
  (route GET "/transactions/:transaction-id/attachments/new" attachments/new-attachment)
  (route POST "/transactions/:transaction-id/attachments" attachments/create)
  (route GET "/attachments/:id/edit" attachments/edit)
  (route POST "/attachments/:id" attachments/update)
  (route POST "/attachments/:id/delete" attachments/delete)

  ; Images
  (route GET "/images/:image-id" images/show)

  ; Trading
  (route GET "/accounts/:account-id/purchases/new" trading/new-purchase)
  (route POST "/accounts/:account-id/purchases" trading/purchase)
  (route GET "/accounts/:account-id/sales/new" trading/new-sale)
  (route POST "/accounts/:account-id/sales" trading/sell)
  (route POST "/transactions/:transaction-id/unbuy" trading/unbuy)

  ; Lots
  (route GET "/accounts/:account-id/lots" lots/index)

  ; Reconciliations
  (route GET "/accounts/:account-id/reconciliations/new" reconciliations/new-reconciliation)
  (route POST "/accounts/:account-id/reconciliations" reconciliations/create)
  (route GET "/reconciliations/:id" reconciliations/show)
  (route GET "/reconciliations/:id/edit" reconciliations/edit)
  (route POST "/reconciliations/:id" reconciliations/update)
  (route POST "/reconciliations/:id/delete" reconciliations/delete)

  ; Imports
  (route GET "/imports/new" imports/new-import)
  
  ; Reports
  (route GET "/entities/:entity-id/reports" reports/render)
  (route GET "/entities/:entity-id/reports/:type" reports/render))

(defroutes api-routes ;TODO Finish setting up these routes
  (route POST "/api/imports" imports-api/create)
  (route GET "/api/imports/:id" imports-api/show))

(defroutes open-routes
  (route GET "/" pages/home)
  (route GET "/login" pages/login)
  (route GET "/signup" users/new-user)
  (route POST "/users" users/create-user))

(defroutes routes
  open-routes
  (friend/logout (POST "/logout" [] (redirect "/")))
  (friend/wrap-authorize protected-routes #{:user})
  (friend/wrap-authorize api-routes #{:user})
  (ANY "*" req
       (do
         (log/debug "unable to match route for " req)
         (route/not-found (slurp (io/resource "404.html"))))))

(def app
  (-> routes
      (friend/authenticate
        {:workflows [(workflows/interactive-form)]
         :credential-fn (partial clj-money.models.users/authenticate (env :db))
         :redirect-on-auth? false})
      (wrap-resource "public")
      wrap-exception-handling
      wrap-params
      wrap-multipart-params
      wrap-json-params
      wrap-keyword-params
      wrap-json-response
      wrap-anti-forgery
      wrap-session))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
