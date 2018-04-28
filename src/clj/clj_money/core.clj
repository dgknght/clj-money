(ns clj-money.core
  (:require [clj-time.format :refer [unparse-local-date
                                     formatters]])
  (:import org.joda.time.LocalDate))

(defmethod print-method LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (unparse-local-date (:year-month-day formatters) this))
    (.write "\"")))
