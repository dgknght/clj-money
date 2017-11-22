(ns clj-money.db
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts]]
            [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rt]
            [environ.core :refer [env]]
            [clj-money.util :refer [parse-local-date]]
            [clj-money.partitioning :refer [create-partition-tables]]))

(defn ragtime-config []
  {:datastore (jdbc/sql-database (env :db))
   :migrations (jdbc/load-resources "migrations")})

(defn migrate
  []
  (rt/migrate (ragtime-config)))

(defn rollback
  []
  (rt/rollback (ragtime-config)))

(def ^:private create-partitions-options
  [["-n" "--dry-run" "Dry run"]
   ["-s" "--silent" "Do not output SQL commands"]])

(defn create-partitions
  [& args]
  (let [{:keys [arguments options]} (parse-opts args create-partitions-options)
        start-date (-> arguments first parse-local-date)
        end-date (or (-> arguments second parse-local-date)
                     start-date)]
    (create-partition-tables start-date end-date options)))
