(ns clj-money.web.server
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
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
            [clj-money.web.users :as users]))

(defroutes protected-routes
  ; Entities
  (GET "/entities" []
       (entities/index))
  (GET "/entities/new" []
       (entities/new-entity))
  (POST "/entities" {params :params}
        (entities/create-entity params))
  (GET "/entities/:id/edit" [id]
       (entities/edit-entity id))
  (POST "/entities/:id" req
        (entities/update (:params req)))

  ; Accounts
  (GET "/accounts" []
       (accounts/index)))

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
  (ANY "*" []
       (route/not-found (slurp (io/resource "404.html")))))

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
