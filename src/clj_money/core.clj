(ns clj-money.core
  (:require [clj-money.dates :as dates]))

(defmethod print-method java.time.LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (dates/serialize-local-date this))
    (.write "\"")))

(defmethod print-method java.time.LocalDateTime [this ^java.io.Writer w]
  (doto w
    (.write "#local-date-time \"")
    (.write (dates/serialize-local-date-time this))
    (.write "\"")))
