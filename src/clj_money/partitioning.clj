; should this really exist in sql-storage?
(ns clj-money.partitioning
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-long
                                     to-local-date]]
            [environ.core :refer [env]]
            [selmer.parser :refer [render]]
            [clj-money.util :refer [ensure-local-date]]))

(defn table-name
  "Given a date and a base table name, returns the name
  of the partition table where the date belongs"
  [date root]
  (let [[year month] ((juxt t/year t/month) (to-local-date date))]
    (format "%s_%04d_%02d" (name root) year month)))

(defn tables-for-range
  [start end root]
  (dedupe [(table-name start root)
           (table-name end root)]))

(def ^:private tables
  [:prices]
  #_[:transactions
   :transaction_items])

(def ^:private create-table-format
  "create table %s (check (transaction_date >= '%s' and transaction_date < '%s')) inherits (%s_base);")

(defn- start-of-period
  "Given a date, returns the date that marks the start of the
  period to which the date belongs"
  [date]
  (t/first-day-of-the-month date))

(defn- start-of-next-period
  "Given a date, returns the date that marks the start of
  the period that follows the period that contains the date"
  [date]
  (t/plus (start-of-period date) (t/months 1)))

(defn- create-table-cmd
  [date table]
  (render (slurp "resources/db/create_partition_table.sql")
          {:table-name (table-name date table)
           :start-of-period (to-long (start-of-period date))
           :start-of-next-period (to-long (start-of-next-period date))
           :base-table-name (format "%s_base" table)}))

(defn- create-table-cmds
  [date]
  (->> tables
       (map name)
       (map #(create-table-cmd date %))))

(defn create-partition-tables
  [date options]
  (jdbc/with-db-connection [c (env :db)]
    (doseq [cmd (create-table-cmds date)]
      (when-not (:silent options)
        (println cmd))
      (when-not (:dry-run options)
        (jdbc/execute! c cmd)))))
