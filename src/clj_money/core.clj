(ns clj-money.core
  (:require [clj-money.dates :as dates]))

(defmethod print-method java.time.LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#clj-money/local-date \"")
    (.write (dates/serialize-local-date this))
    (.write "\"")))

(defmethod print-method java.time.LocalDateTime [this ^java.io.Writer w]
  (doto w
    (.write "#clj-money/local-date-time \"")
    (.write (dates/serialize-local-date-time this))
    (.write "\"")))

(defmethod print-method java.time.Instant [this ^java.io.Writer w]
  (let [s (.format (-> (java.time.format.DateTimeFormatter/ofPattern
                         "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
                       (.withZone java.time.ZoneOffset/UTC))
                   this)]
    (doto w
      (.write "#clj-money/instant \"")
      (.write s)
      (.write "\""))))
