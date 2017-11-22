; should this really exist in sql-storage?
(ns clj-money.partitioning
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date]]
            [clj-time.periodic :refer [periodic-seq]]
            [environ.core :refer [env]]
            [selmer.parser :refer [render]]
            [clj-money.util :refer [to-sql-date]]))

(defn- table-suffix
  "Given a date, returns the suffix for the table where the
  date belongs"
  [date]
  (if-not date
    (throw (IllegalArgumentException. "Argument \"date\" cannot be nil")))
  (let [[year month] ((juxt t/year t/month) (to-local-date date))]
    (format "_%04d_%02d" year month)))

(defn table-name
  "Given a date and a base table name, returns the name
  of the partition table where the date belongs"
  [date root]
  (format "%s%s" (name root) (table-suffix date)))

; This part if specific to the strategy for
; segementing the data. E.g., monthly, quarterly, anually, etc.
; This implementionis monthly
(defn- partition-dates
  "Given any two dates, calculates marker dates
  for each partition period required to store dates
  in the range"
  [start-date end-date]
  (periodic-seq (t/first-day-of-the-month start-date)
                (t/last-day-of-the-month end-date)
                (t/months 1)))

(defn tables-for-range
  [start end root]
  (->> (partition-dates (to-local-date start) (to-local-date end))
       (map #(table-name % (name root)))))

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
  (render (slurp (format "resources/db/create_partition_table_%s.sql" table))
          {:table-suffix (table-suffix date)
           :start-of-period (to-local-date (start-of-period date))
           :start-of-next-period (to-local-date (start-of-next-period date))}))

(defn- create-table-cmds
  "Given any two dates, calculates the tables that need to
  be created to accomodate data within the range and returns
  the commands to create them"
  [start-date end-date]
  (let [dates (partition-dates start-date end-date)]
    (->> tables
         (map name)
         (mapcat (fn [table]
                   (map #(create-table-cmd % table)
                        dates))))))

(defn create-partition-tables
  "Creates the specified partition tables.

  Arguments:
    start-date - the start of the range for which tables are to be created
    end-date   - the end of the range for which tables are to be created
  Options:
    :silent    - do not output the commands that are generated
    :dry-run   - do not execute the commands that are generated"
  [start-date end-date options]
  (jdbc/with-db-connection [c (env :db)]
    (doseq [cmd (create-table-cmds start-date end-date)]
      (when-not (:silent options)
        (println cmd))
      (when-not (:dry-run options)
        (jdbc/execute! c cmd)))))
