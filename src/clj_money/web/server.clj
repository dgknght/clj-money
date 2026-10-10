(ns clj-money.web.server
  "Runs the web server as an Integrant component and provides the
  application's entry point."
  (:require [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [ring.adapter.jetty :as jetty]
            [clj-money.config :refer [env]]
            [clj-money.system :as system]
            [clj-money.web :as-alias web])
  (:import org.eclipse.jetty.server.Server))

(defmethod ig/init-key ::web/server
  [_ {:keys [handler port]}]
  (println (format "Starting web server on port %s..." port))
  (log/infof "Starting web server on port %s" port)
  (let [server (jetty/run-jetty handler {:port port :join? false})]
    (log/infof "Web server listening on port %s" port)
    (println (format "Web server listening on port %s." port))
    server))

(defmethod ig/halt-key! ::web/server
  [_ ^Server server]
  (log/info "Stopping web server")
  (.stop server))

(defn- exit
  [status]
  (System/exit status))

(defn -main
  "Starts the full system, and halts it when the JVM shuts down. Exits with
  a non-zero status if the system fails to start."
  [& [port]]
  (try
    (let [sys (system/init (system/config (cond-> env
                                            port (assoc :port port))))]
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. (fn []
                                   (log/info "Halting the system")
                                   (system/halt sys))))
      sys)
    (catch Throwable e
      (log/fatal e "Unable to start the system")
      (exit 1))))
