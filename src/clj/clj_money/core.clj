(ns clj-money.core
  (:require [java-time.api :as t]
            [dgknght.app-lib.web :refer [serialize-date]]))

(defmethod print-method org.joda.time.LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#joda-local-date \"")
    (.write (serialize-date this))
    (.write "\"")))

(defmethod print-method java.time.LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (t/format (t/formatter :iso-date) this))
    (.write "\"")))
