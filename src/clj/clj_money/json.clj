(ns clj-money.json
  (:refer-clojure :exclude [update])
  (:require [cheshire.generate :refer [add-encoder]]
            [clj-money.dates :refer [serialize-local-date]]))

(add-encoder
  java.time.LocalDate
  (fn [date gen]
    (.writeString gen (serialize-local-date date))))
