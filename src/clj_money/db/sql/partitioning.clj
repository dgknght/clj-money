(ns clj-money.db.sql.partitioning
  (:require [next.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.config :refer [env]]
            [clj-money.dates :as dates]))

(defmulti ^:private suffix :interval-type)

(defmethod ^:private suffix :year
  [{:keys [interval-count] [start-date next-start-date] :dates}]
  (if (= 1 interval-count)
    (format "_y%s" (dates/year start-date))
    (format "_y%s_y%s"
            (dates/year start-date)
            (dates/year (t/minus next-start-date
                                 (t/days 1))))))

(defmethod ^:private suffix :month
  [{:keys [interval-count] [start-date next-start-date] :dates}]
  (let [[year month] ((juxt dates/year dates/month) start-date)]
    (if (= 1 interval-count)
      (format "_y%04d_m%02d" year month)
      (format "_y%04d_m%02d_m%02d"
              year
              month
              (dates/month (t/minus next-start-date
                                    (t/days 1))))))) ; assuming we won't cross a year boundary here

(defmulti period-like :interval-type)

(defmethod period-like :month
  [{:keys [interval-count]}]
  (t/months interval-count))

(defmethod period-like :year
  [{:keys [interval-count]}]
  (t/years interval-count))

(def ^:private tables
  {:price {:interval-type :year
           :interval-count 1}
   :cached_price {:interval-type :year
                  :interval-count 1}
   :transaction {:interval-type :year
                 :interval-count 1}
   :transaction_item {:interval-type :year
                      :interval-count 1}
   :account_item {:interval-type :year
                  :interval-count 1}
   :reconciliation {:interval-type :year
                    :interval-count 5}})

(defmulti ^:private period-range :interval-type)

(defmethod period-range :year
  [{:keys [date]}]
  (let [start-of-period (t/local-date (dates/year date) 1 1)]
    [start-of-period                         ; The lower boundary is inclusive
     (t/plus start-of-period (t/years 1))])) ; The upper boundary is exclusive

(defmethod period-range :month
  [{:keys [date]}]
  (let [start-of-period (dates/first-day-of-the-month date)]
    [start-of-period
     (t/plus start-of-period (t/months 1))]))

(defn- create-table-cmd
  [{:keys [table-name dates suffix]}]
  (format
   "create table if not exists %s%s partition of %s for values from ('%s') to ('%s');"
   table-name
   suffix
   table-name
   (first dates)
   (second dates)))

(defmulti ^:private anchor
  "Given a date and an interval count, return the first valid starting date for
  the combination.

  In order to support partitions that span multiple years, we need to ensure
  that we are not creating overlaps."
  (fn [_date {:keys [interval-type]}]
    interval-type))

(def ^:private anchor-point (t/local-date 2001 1 1))

(defmethod ^:private anchor :year
  [date {:keys [interval-count]}]
  (if (= 1 interval-count)
    date
    (let [offset (mod (Math/abs (- (dates/year date)
                                   (dates/year anchor-point)))
                      interval-count)]
      (t/minus date (t/years offset)))))

(defmethod ^:private anchor :month
  [date {:keys [interval-count]}]
  (if (= 1 interval-count)
    date
    (let [offset (mod (.getMonths
                        (apply t/period
                               (sort #(t/before? %1 %2)
                                     [anchor-point date])))
                      interval-count)]
      (t/minus date (t/months offset)))))

(defn- create-table-cmds
  "Given any two dates, calculates the tables that need to
  be created to accomodate data within the range and returns
  the commands to create them"
  [start-date end-date options]
  (->> tables
       (map (comp #(merge % (get-in options [:rules (:table %)])) ; allow for override of default rules
                  #(assoc (second %)
                          :table (first %)
                          :table-name (name (first %))))) ; turn the k-v pairs into a map
       (mapcat (fn [opts]
                 (->> (dates/periodic-seq (anchor start-date opts)
                                          (period-like opts))
                      (partition 2 1)
                      (take-while #(t/before? (first %) end-date))
                      (map #(assoc opts :dates %)))))
       (map #(assoc % :suffix (suffix %)))
       (map create-table-cmd)))

(defn- sql-config []
  (assoc (get-in env [:db :strategies :sql])
         :user (env :sql-ddl-user) 
         :password (env :sql-ddl-password)))

(defn create-partition-tables
  "Creates the specified partition tables.

  Arguments:
    start-date - the start of the range for which tables are to be created
    end-date   - the end of the range for which tables are to be created
  Options:
    :silent    - do not output the commands that are generated
    :dry-run   - do not execute the commands that are generated
    :rules     - a map of table names to interval type and count"
    ([start-date end-date options]
     (create-partition-tables (sql-config) start-date end-date options))
    ([config start-date end-date options]
     (let [ds (jdbc/get-datasource config)]
       (doseq [cmd (create-table-cmds start-date end-date options)]
         (when-not (:silent options)
           (println cmd))
         (when-not (:dry-run options)
           (jdbc/execute! ds [cmd]))))))
