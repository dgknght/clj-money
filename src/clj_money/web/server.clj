(ns clj-money.web.server
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer [defroutes GET PUT POST DELETE ANY]]
            [compojure.handler :refer [site]]
            [compojure.route :as route]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.util.response :refer [redirect]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [cemerick.friend.credentials :as creds])
  (:use [clj-money.web.pages :as pages]
        [clj-money.web.accounts :as accounts]))

(defroutes app
  (GET "/" []
       (pages/home))
  (GET "/login" []
       (pages/login))
  (GET "/accounts" []
       (friend/authorize #{::user} (accounts/index)))
  (friend/logout (POST "/logout" [] (redirect "/")))
  (ANY "*" []
       (route/not-found (slurp (io/resource "404.html")))))

; TODO Replace this with a database implementation
(def users
  {"doug" {:username "doug"
           :password (creds/hash-bcrypt "please01")
           :roles #{::user}}})

(def secured-app
  (-> app
      (friend/authenticate {:workflows [(workflows/interactive-form)]
                            :credential-fn (partial creds/bcrypt-credential-fn users)})
      (wrap-keyword-params)
      (wrap-params)
      (wrap-session)))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'secured-app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
