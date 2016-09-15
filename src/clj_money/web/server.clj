(ns clj-money.web.server
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer [defroutes GET PUT POST DELETE ANY]]
            [compojure.handler :refer [site]]
            [compojure.route :as route]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [cemerick.friend.credentials :as creds])
  (:use [clj-money.web.pages :as pages]))

(defroutes app
  (GET "/" []
       (pages/home))
  (GET "/login" []
       (pages/login))
  (ANY "*" []
       (route/not-found (slurp (io/resource "404.html")))))

; TODO Replace this with a database implementation
(def users
  {"doug" {:username "doug"
           :password (creds/hash-bcrypt "please01")
           :roles #{::admin}}})

(def secured-app
  (-> app
      (friend/authenticate {:credential-fn (partial creds/bcrypt-credential-fn users)
                            :workflows [(workflows/interactive-form)]})))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'secured-app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))
