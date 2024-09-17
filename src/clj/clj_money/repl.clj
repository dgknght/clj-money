(ns clj-money.repl
  (:require [clj-money.web.server :as s]
            [clj-money.models.sql-storage]
            [clj-money.models.users :as usrs]))

(def server (atom nil))

(defn start-server []
  (reset! server (s/-main)))

(defn stop-server []
  (.stop @server)
  (reset! server nil))

(defn create-user
  [& {:as args}]
  (usrs/create args))
