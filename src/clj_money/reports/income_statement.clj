(ns clj-money.reports.income-statement
  (:require [java-time.api :as t]
            [clj-money.entities :as entities]
            [clj-money.dates :as dates]
            [clj-money.accounts :refer [nest]]
            [clj-money.reports :as rpt]))

(defn- summarize
  [records]
  (let [{:keys [income expense]} (rpt/map-record-headers records)]
    (concat (map #(dissoc % :report/type) records)
            [{:report/caption "Net"
              :report/value (- income expense)
              :report/style :summary}])))

(defn- default-range []
  (let [start (dates/first-day-of-the-month (t/local-date))]
    [start (t/plus start (t/months 1))]))

(defn report
  "Returns the data used to populate an income statement report"
  ([entity]
   (let [[since as-of] (default-range)]
     (report entity since as-of)))
  ([entity since as-of]
   (->> (entities/select #:account{:entity entity
                                   :type [:in #{:income :expense}]})
        (rpt/valuate-accounts {:entity entity
                               :since since
                               :as-of as-of})
        (nest {:types [:income :expense]})
        (mapcat rpt/summarize-account-type)
        summarize)))
