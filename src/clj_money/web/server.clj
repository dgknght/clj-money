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
  (GET "/authenticate" [username password]
       (pages/authenticate username password))
  (GET "/accounts" []
       (friend/authorize #{::user} (accounts/index)))
  (ANY "*" []
       (route/not-found (slurp (io/resource "404.html")))))

(def secured-app
  (-> app
      (friend/authenticate {:credential-fn (partial creds/bcrypt-credential-fn users)
                            :workflows [(workflows/interactive-form)]})
      (wrap-keyword-params)
      (wrap-params)
      (wrap-session)))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'secured-app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
