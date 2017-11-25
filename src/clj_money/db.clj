(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-time.core :as t]
            [clojure.java.jdbc :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :refer [select
                                      update
                                      sset
                                      from
                                      where
                                      values
                                      insert-into]]
            [ragtime.jdbc :refer [sql-database
                                  load-resources]]
            [ragtime.repl :as rt]
            [environ.core :refer [env]]
            [clj-money.core]
            [clj-money.util :refer [parse-local-date]]
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

(defn- update-earliest-partition-date
  [date]
  (let [setting-name "earliest-partition-date"
        existing (when-let [record (first (jdbc/query (env :db) (-> (select :value)
                                                                    (from :settings)
                                                                    (where [:= :name setting-name])
                                                                    sql/format)))]
                   (read-string (:value record)))
        sql (if existing
              (when (t/before? date existing)
                (-> (update :settings)
                    (sset {:value (prn-str date)})
                    (where [:= :name setting-name])))
              (-> (insert-into :settings)
                  (values [{:name setting-name
                            :value (prn-str date)}])))]
    (when sql
      (jdbc/execute! (env :db) (sql/format sql)))))

(defn create-partitions
  [& args]
  (let [{:keys [arguments options]} (parse-opts args create-partitions-options)
        start-date (-> arguments first parse-local-date)
        end-date (or (-> arguments second parse-local-date)
                     start-date)]
    (create-partition-tables start-date end-date options)
    (update-earliest-partition-date start-date)))
