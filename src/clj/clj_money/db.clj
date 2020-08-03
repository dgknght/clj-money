(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require[clojure.tools.cli :refer [parse-opts]]
            [clj-time.core :as t]
            [ragtime.jdbc :refer [sql-database
                                  load-resources]]
            [ragtime.repl :as rt]
            [environ.core :refer [env]]
            [clj-money.core]
            [clj-money.util :refer [unserialize-date]]
            [clj-money.models.settings :as settings]
            [clj-money.partitioning :refer [create-partition-tables]]))

(defn ragtime-config []
  {:datastore (sql-database (env :db))
   :migrations (load-resources "migrations")})

(defn migrate
  []
  (rt/migrate (ragtime-config)))

(defn rollback
  []
  (rt/rollback (ragtime-config)))

(def ^:private create-partitions-options
  [["-n" "--dry-run" "Dry run"]
   ["-s" "--silent" "Do not output SQL commands"]])

(defn put-partition-date
  [setting-name date compare-fn]
  (when (if-let [existing (settings/get (env :db) setting-name)]
          (when (compare-fn date (:value existing))
            date)
          date)
    (settings/put (env :db) setting-name date)))

(defn- put-earliest-partition-date
  [date]
  (put-partition-date "earliest-partition-date"
                      date
                      t/before?))

(defn- put-latest-partition-date
  [date]
  (put-partition-date "latest-partition-date"
                      date
                      t/after?))

(defn create-partitions
  [& args]
  (let [{:keys [arguments options]} (parse-opts args create-partitions-options)
        start-date (-> arguments first unserialize-date)
        end-date (or (-> arguments second unserialize-date)
                     start-date)]
    (create-partition-tables start-date end-date options)
    (put-earliest-partition-date start-date)
    (put-latest-partition-date end-date)))
