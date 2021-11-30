(ns clj-money.json
  (:refer-clojure :exclude [update])
  (:require [cheshire.generate :refer [add-encoder]]
            [dgknght.app-lib.web :refer [serialize-date
                                         serialize-date-time]]))

(add-encoder
  org.joda.time.DateTime
  (fn [date-time gen]
    (.writeString gen (serialize-date-time date-time))))

(add-encoder
 org.joda.time.LocalDate
 (fn [local-date gen]
   (.writeString gen (serialize-date local-date))))
