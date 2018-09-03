(ns clj-money.core
  (:require [clj-time.format :refer [unparse-local-date
                                     formatters]])
  (:import org.joda.time.LocalDate
           [com.fasterxml.jackson.databind ObjectMapper]
           [com.fasterxml.jackson.datatype.joda JodaModule]))

(defmethod print-method LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (unparse-local-date (:year-month-day formatters) this))
    (.write "\"")))

; Register fasterxml joda module
(.registerModule (ObjectMapper.) (JodaModule.))
