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

(defroutes all-routes
  ; Entities
  (GET "/entities" req entities/index)
  (GET "/entities/new" req entities/new-entity)
  (POST "/entities" req entities/create-entity)
  (GET "/entities/:id/edit" req entities/edit-entity)
  (POST "/entities/:id" req entities/update)
  (POST "/entities/:id/delete" req entities/delete)

  ; User grants
  (GET "/entities/:entity-id/grants" req grants/index)
  (GET "/entities/:entity-id/grants/new" req grants/new-grant)
  (POST "/entities/:entity-id/grants" req grants/create)
  (GET "/grants/:id/edit" req grants/edit)
  (POST "/grants/:id" req grants/update)
  (POST "/grants/:id/delete" req grants/delete)

  ; Accounts
  (GET "/entities/:entity-id/accounts" req accounts/index)
  (GET "/entities/:entity-id/accounts/new" req accounts/new-account)
  (POST "/entities/:entity-id/accounts" req accounts/create)
  (GET "/accounts/:id" req accounts/show)
  (GET "/accounts/:id/edit" req accounts/edit)
  (POST "/accounts/:id" req accounts/update)
  (POST "/accounts/:id/delete" req accounts/delete)

  ; Budgets
  (GET "/entities/:entity-id/budgets" req budgets/index)
  (GET "/entities/:entity-id/budgets/new" req budgets/new-budget)
  (POST "/entities/:entity-id/budgets" req budgets/create)
  (GET "/budgets/:id/edit" req budgets/edit)
  (POST "/budgets/:id" req budgets/update)
  (POST "/budgets/:id/delete" req budgets/delete)
  (GET "/budgets/:id" req budgets/show)

  ; Budget items
  (GET "/budgets/:budget-id/items/new/:method" req budgets/new-item)
  (POST "/budgets/:budget-id/items" req budgets/create-item)
  (GET "/budget-items/:id/edit/:method" req budgets/edit-item)
  (POST "/budget-items/:id" req budgets/update-item)
  (POST "/budget-items/:id/delete" req budgets/delete-item)

  ; Budget monitors
  (GET "/entities/:entity-id/monitors" req entities/monitors)
  (POST "/entities/:entity-id/monitors" req entities/create-monitor)
  (POST "/entities/:entity-id/monitors/:account-id/delete" req entities/delete-monitor)

  ; Commodities
  (GET "/entities/:entity-id/commodities" req commodities/index)
  (GET "/entities/:entity-id/commodities/new" req commodities/new-commodity)
  (POST "/entities/:entity-id/commodities" req commodities/create)
  (GET "/commodities/:id/edit" req commodities/edit)
  (POST "/commodities/:id" req commodities/update)
  (POST "/commodities/:id/delete" req commodities/delete)

  ; Prices
  (GET "/commodities/:commodity-id/prices" req prices/index)
  (GET "/commodities/:commodity-id/prices/new" req prices/new-price)
  (POST "/commodities/:commodity-id/prices" req prices/create)
  (POST "/commodities/:commodity-id/prices/fetch" req prices/fetch)
  (POST "/entities/:entity-id/prices/fetch" req prices/fetch-all)
  (POST "/prices/:id/delete" req prices/delete)

  ; Transactions
  (GET "/entities/:entity-id/transactions" req transactions/index)
  (GET "/entities/:entity-id/transactions/new" req transactions/new-transaction)
  (POST "/entities/:entity-id/transactions" req transactions/create)
  (GET "/transactions/:transaction-date/:id/edit" req transactions/edit)
  (POST "/transactions/:transaction-date/:id" req transactions/update)
  (POST "/transactions/:transaction-date/:id/delete" req transactions/delete)

  ; Attachments
  (GET "/transactions/:transaction-id/attachments" req attachments/index)
  (GET "/transactions/:transaction-id/attachments" req attachments/index)
  (GET "/transactions/:transaction-id/attachments/new" req attachments/new-attachment)
  (POST "/transactions/:transaction-id/attachments" req attachments/create)
  (GET "/attachments/:id/edit" req attachments/edit)
  (POST "/attachments/:id" req attachments/update)
  (POST "/attachments/:id/delete" req attachments/delete)

  ; Images
  (GET "/images/:image-id" req images/show)

  ; Trading
  (GET "/accounts/:account-id/purchases/new" req trading/new-purchase)
  (POST "/accounts/:account-id/purchases" req trading/purchase)
  (GET "/accounts/:account-id/sales/new" req trading/new-sale)
  (POST "/accounts/:account-id/sales" req trading/sell)
  (POST "/transactions/:transaction-id/unbuy" req trading/unbuy)

  ; Lots
  (GET "/accounts/:account-id/lots" req lots/index)

  ; Reconciliations
  (GET "/accounts/:account-id/reconciliations/new" req reconciliations/new-reconciliation)
  (POST "/accounts/:account-id/reconciliations" req reconciliations/create)
  (GET "/reconciliations/:id" req reconciliations/show)
  (GET "/reconciliations/:id/edit" req reconciliations/edit)
  (POST "/reconciliations/:id" req reconciliations/update)
  (POST "/reconciliations/:id/delete" req reconciliations/delete)

  ; Imports
  (GET "/imports/new" req imports/new-import)
  
  ; Reports
  (GET "/entities/:entity-id/reports" req reports/render)
  (GET "/entities/:entity-id/reports/:type" req reports/render)

  ; Single page apps
  (GET "/apps" req apps/index)
  (GET "/apps/:id" req apps/show)

  (GET "/api/entities" req entities-api/index)
  (DELETE "/api/entities/:id" req entities-api/delete)

  ; Imports
  (POST "/api/imports" req imports-api/create)
  (GET "/api/imports/:id" req imports-api/show)

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
      (when (test-fn request)
        (pprint {(str "enter " description " " request-method " " uri) (select-keys request [:cookies :session :headers])}))
      (let [apply? (test-fn request)
            working-handler (if apply?
                              (wrapper-fn handler)
                              handler)
            response (working-handler request)]
        (when apply?
          (pprint {(str "exit " description " " request-method " " uri) (select-keys response [:cookies :session :headers])}))
        response))))

(defn-  wrap-routes
  [handler & wrappers]
  (reduce apply-wrapper
          handler
          wrappers))

(def wrapped-routes
  (wrap-routes all-routes
               [wrap-models                         "models"]
               [wrap-integer-id-params              "multipart params"]
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
               [wrap-session                        "session"]
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
