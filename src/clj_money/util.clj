(ns clj-money.util
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-time.core :as t])
  (:import java.text.DecimalFormat
           org.joda.time.format.DateTimeFormat))

(def number-formats
  {:standard (DecimalFormat. "#,##0.00")
   :no-comma (DecimalFormat. "0.00")
   :integer  (DecimalFormat. "0")})

(defn format-number
  "Format a number with 2 decimal places and groups separated with commas"
  ([value] (format-number value {}))
  ([value options]
   (when value
     (.format (number-formats (or (:format options) :standard))
              value))))

(def DateFormat (DateTimeFormat/forPattern "M/d/y"))

(defn format-date
  "Formats a date (without time) in the standard US format"
  [value]
  (.print DateFormat value))

(def date-patterns
  [{:pattern #"(\d{1,2})/(\d{1,2})/(\d{4})"
    :groups [:month :day :year]}
   {:pattern
    #"(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})"
    :groups [:year :month :day]}])

(defn parse-date
  "Parses the specified date value"
  [date-string]
  (when date-string
    (when-let [parsed (some (fn [{:keys [pattern groups]}]
                              (when-let [m (re-matches pattern date-string)]
                                (zipmap groups (->> m
                                                    rest
                                                    (map #(Integer. %))))))
                            date-patterns)]
      (apply t/local-date ((juxt :year :month :day) parsed)))))

(defn pprint-and-return
  [message value]
  (println "")
  (println message)
  (pprint value)
  value)
