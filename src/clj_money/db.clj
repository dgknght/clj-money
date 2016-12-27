(ns clj-money.db
  (:require [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rt]
            [environ.core :refer [env]]))

(defn ragtime-config []
  {:datastore (jdbc/sql-database (env :db))
   :migrations (jdbc/load-resources "migrations")})

(defn migrate
  []
  (rt/migrate (ragtime-config)))

(defn rollback
  []
  (rt/rollback (ragtime-config)))
