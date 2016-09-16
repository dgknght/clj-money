(ns clj-money.db
  (:require [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rt]
            [environ.core :refer [env]])
  #_(:use [clj-money.config :as config]))

#_(defn ragtime-config []
  {:datastore (jdbc/database (:db config))
   :migrations (jdbc/load-resources "db/migrations")})

(defn migrate
  []
  (println (str "this is a migration for " (env :database-url)))
  #_(rt/migrate (ragtime-config)))

(defn rollback
  []
  (println (str "this is a rollback for " (env :database-url)))
  #_(rt/rollback (ragtime-config)))
