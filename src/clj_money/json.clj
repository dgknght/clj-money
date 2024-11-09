(ns clj-money.json
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [cheshire.generate :refer [add-encoder]]
            [clj-money.dates :refer [serialize-local-date]]))

(add-encoder
  java.time.LocalDate
  (fn [date gen]
    (.writeString gen (serialize-local-date date))))

(add-encoder
  BigDecimal
  (fn [d gen]
    (.writeString gen (format "%.2f" d))))
