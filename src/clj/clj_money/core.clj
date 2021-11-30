(ns clj-money.core
  (:require [dgknght.app-lib.web :refer [serialize-date]])
  (:import org.joda.time.LocalDate))

(defmethod print-method LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (serialize-date this))
    (.write "\"")))
