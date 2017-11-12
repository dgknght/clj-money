(ns clj-money.util
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [clj-time.format :refer [parse-local
                                     formatter]])
  (:import java.text.DecimalFormat
           org.joda.time.format.DateTimeFormat
           org.joda.time.LocalDate))

(def number-formats
  {:standard (DecimalFormat. "#,##0.00")
   :no-comma (DecimalFormat. "0.00")
   :integer  (DecimalFormat. "0")
   :commodity-price (DecimalFormat. "#,##0.000")})

(defn format-number
  "Format a number with 2 decimal places and groups separated with commas"
  ([value] (format-number value {}))
  ([value options]
   (try
   (when value
     (.format (number-formats (or (:format options) :standard))
              value))
   (catch Exception e
     (log/warn "Unable to format number \"" value "\"")))))

(def DateFormat (DateTimeFormat/forPattern "M/d/y"))

(defn format-date
  "Formats a date (without time) in the standard US format"
  [value]
  (try
    (.print DateFormat value)
    (catch Exception e
      (log/warn "Unable to format date value \"" value "\""))))

(defn parse-local-date
  "Parses the specified date value"
  [date-string]
  (when date-string
    (or (parse-local date-string)
                 (parse-local (formatter "M/d/Y") date-string))))

(defn ensure-local-date
  "Ensures that the specified value is a local date"
  [value]
  (cond
    (instance? LocalDate value)
    value

    (string? value)
    (parse-local-date value)))

(defn to-sql-date
  [value]
  (when value
    (java.sql.Date. (.getTime (.toDate value)))))

(defn pprint-and-return
  [message value]
  (println "")
  (println message)
  (when (meta value)
    (pprint {:meta (meta value)}))
  (println "type " (type value))
  (pprint value)
  value)

(defn pprint-and-return-l
  [value message]
  (pprint-and-return message value))

(defmacro safe-invoke
  "Executes a function accepting a single argument only if
  the argument is not nil, in which case nil is returned"
  [target-fn value]
  `(when ~value
     (~target-fn ~value)))

(defn keywordify-keys
  [m]
  (when m
    (into {} (map (fn [[k v]]
                    [(keyword k)
                     (cond
                       (map? v)
                       (keywordify-keys v)

                       (sequential? v)
                       (map keywordify-keys v)

                       :else v)])
                  m))))

(defn safe-read-string
  [string]
  (read-string (or string "{}")))
