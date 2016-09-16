(ns clj-money.db
  (:require [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rt])
  #_(:use [clj-money.config :as config]))

#_(defn ragtime-config []
  {:datastore (jdbc/database (:db config))
   :migrations (jdbc/load-resources "db/migrations")})

(defn migrate
  []
  (println "this is a migration")
  #_(rt/migrate (ragtime-config)))

(defn rollback
  []
  (println "this is a rollback")
  #_(rt/rollback (ragtime-config)))
