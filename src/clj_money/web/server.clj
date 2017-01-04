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
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :refer [redirect]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [cemerick.friend.credentials :as creds]
            [clj-money.web.pages :as pages]
            [clj-money.web.entities :as entities]
            [clj-money.web.accounts :as accounts]
            [clj-money.web.budgets :as budgets]
            [clj-money.web.transactions :as transactions]
            [clj-money.web.reports :as reports]
            [clj-money.web.users :as users]))

(defmacro route
  [method path & handlers]
  `(~method ~path req# (->> req# ~@handlers)))

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

  ; Transactions
  (GET "/entities/:entity-id/transactions" [entity-id]
       (transactions/index (Integer. entity-id)))
  (GET "/entities/:entity-id/transactions/new" {params :params}
       (transactions/new-transaction (update-in params [:entity-id] #(Integer. %))))
  (POST "/entities/:entity-id/transactions" {params :params}
        (transactions/create (update-in params [:entity-id] #(Integer. %))))
  (GET "/transactions/:id/edit" [id redirect]
       (transactions/edit (Integer. id) {:redirect redirect}))
  (POST "/transactions/:id" req
        (transactions/update (:params req)))
  (POST "/transactions/:id/delete" {params :params}
        (transactions/delete (update-in params [:id] #(Integer. %))))
  
  ; Reports
  (GET "/entities/:entity-id/reports" [entity-id]
       (reports/render {:entity-id entity-id :type :income-statement}))
  (GET "/entities/:entity-id/reports/:type" {params :params}
       (reports/render params)))

(defroutes routes
  (GET "/" []
       (pages/home))
  (GET "/login" []
       (pages/login))
  (GET "/signup" []
       (users/new-user))
  (POST "/users" req
        (users/create-user (:params req)))
  (friend/logout (POST "/logout" [] (redirect "/")))
  (friend/wrap-authorize protected-routes #{:user})
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
      (wrap-keyword-params)
      (wrap-params)
      (wrap-session)))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
