(ns clj-money.core
  (:require [java-time.api :as t]))

(defmethod print-method java.time.LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (t/format (t/formatter :iso-date) this))
    (.write "\"")))

(defmethod print-method java.time.LocalDateTime [this ^java.io.Writer w]
  (doto w
    (.write "#local-date-time \"")
    (.write (t/format (t/formatter :iso-date-time) this))
    (.write "\"")))
