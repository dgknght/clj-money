(ns clj-money.json
  (:refer-clojure :exclude [update])
  (:require [cheshire.generate :refer [add-encoder]]
            [clj-time.format :as tf]))

(add-encoder org.joda.time.DateTime
             (fn [date-time json-generator]
               (.writeString
                json-generator
                (tf/unparse (:ordinal-date-time tf/formatters)
                            date-time))))
