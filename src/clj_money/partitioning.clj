; should this really exist in sql-storage?
(ns clj-money.partitioning
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date]]
            [clj-time.periodic :refer [periodic-seq]]
            [environ.core :refer [env]]
            [selmer.parser :refer [render]]
            [clj-money.util :refer [to-sql-date
                                    pprint-and-return]]))

(defn partition-period
  [& _]
  (keyword (env :partition-period)))

(defmulti table-suffix* partition-period)

(defmethod table-suffix* :year
  [date]
  (format "_%s" (t/year date)))

(defmethod table-suffix* :month
  [date]
  (let [[year month] ((juxt t/year t/month) (to-local-date date))]
    (format "_%04d_%02d" year month)))

(defn table-suffix
  [date]
  (if-not date
    (throw (IllegalArgumentException. "Argument \"date\" cannot be nil")))
  (table-suffix* (to-local-date date)))

(defn table-name
  "Given a date and a base table name, returns the name
  of the partition table where the date belongs"
  [date root]
  (keyword (format "%s%s" (name root) (table-suffix date))))

(defn- descending-periodic-seq
  ([end period-like]
   (let [period (.toPeriod period-like)]
     (map (fn [i]

            (log/debug "dbk descending-periodic-seq " end " index " i)

            (t/minus end (.multipliedBy period i)))
          (iterate inc 0))))
  ([start end period-like]
   (let [period (.toPeriod period-like)]
     (take-while (fn [next]
                   (t/after? next start))
                 (descending-periodic-seq end period-like)))))

(defmulti ^:private partition-dates partition-period)

(defmethod partition-dates :month
  [start-date end-date options]
  (let [seq-fn (if (:descending? options)
                 descending-periodic-seq
                 periodic-seq)]
    (seq-fn (t/first-day-of-the-month start-date)
            (t/last-day-of-the-month end-date)
            (t/months 1))))

(defmethod partition-dates :year
  [start-date end-date options]
  (let [seq-fn (if (:descending? options)
                 descending-periodic-seq
                 periodic-seq)]
    (seq-fn (t/local-date (t/year start-date) 1 1)
            (t/local-date (t/year end-date) 12 31)
            (t/years 1))))

(defn tables-for-range
  [start end root options]
  (->> (partition-dates (to-local-date start)
                        (to-local-date end)
                        options)
       (map #(table-name % (name root)))))

(def ^:private tables
  [:prices
   :transactions
   :transaction_items])

(def ^:private create-table-format
  "create table %s (check (transaction_date >= '%s' and transaction_date < '%s')) inherits (%s_base);")

(defmulti ^:private period-range partition-period)

(defmethod period-range :year
  [date]
  (let [start-of-period (t/local-date (t/year date) 1 1)]
    [start-of-period
     (t/plus start-of-period (t/years 1))]))

(defmethod period-range :month
  [date]
  (let [start-of-period (t/first-day-of-the-month date)]
    [start-of-period
     (t/plus start-of-period (t/months 1))]))

(defn- create-table-cmd
  [date table]
  (let [[start next-start] (period-range date)]
  (render (slurp (format "resources/db/create_partition_table_%s.sql" table))
          {:table-suffix (table-suffix date)
           :start-of-period start
           :start-of-next-period next-start})))

(defn- create-table-cmds
  "Given any two dates, calculates the tables that need to
  be created to accomodate data within the range and returns
  the commands to create them"
  [start-date end-date]
  (let [dates (partition-dates start-date end-date {})]
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
