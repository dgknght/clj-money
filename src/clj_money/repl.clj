(ns clj-money.repl
  (:require [clj-money.web.server :as s]))

(def server (atom nil))

(defn start-server []
  (reset! server (s/-main)))

(defn stop-server []
  (.stop @server)
  (reset! server nil))
