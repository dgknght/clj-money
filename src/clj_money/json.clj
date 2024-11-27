(ns clj-money.json
  (:require [clojure.pprint :refer [pprint]]
            [cheshire.generate :refer [add-encoder]]
            [clj-money.dates :refer [serialize-local-date
                                     serialize-local-date-time]]))

(add-encoder
  java.time.LocalDate
  (fn [date gen]
    (.writeString gen (serialize-local-date date))))

(add-encoder
  java.time.LocalDateTime
  (fn [date-time gen]
    (.writeString gen (serialize-local-date-time date-time))))
