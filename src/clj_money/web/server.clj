(ns clj-money.web.server
  (:require [clojure.tools.logging :as log]
            [ring.adapter.jetty :as jetty]
            [clj-money.config :refer [env]]
            [clj-money.system :as system]
            [clj-money.web :as-alias web]))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 3000))
        sys (system/init)]
    (println (format "Starting web server on port %s..." port))
    (log/infof "Starting web server on port %s" port)
    (let [server (jetty/run-jetty (::web/handler sys) {:port port :join? false})]
      (log/infof "Web server listening on port %s" port)
      (println (format "Web server listening on port %s." port))
      server)))
