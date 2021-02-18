(ns clj-money.models.date-helpers
  (:require [clj-time.core :as t]
            [clj-time.coerce :refer [to-long]]
            [clj-money.dates :as dates]
            [clj-money.models.settings :as settings]))

(defmulti date-range
  "Accepts a date criteria value and returns a tuple
  containing the start date in the first position and
  the end date in the second position"
  (fn [range-value]
    (if (sequential? range-value)
      (case (first range-value)
        :between :ternary
        :and :as-is
        :binary)
      :scalar)))

(defn earliest-date []
  (settings/get :earliest-partition-date))

(defn- latest-date []
  (settings/get :latest-partition-date))

(defmethod date-range :as-is ; This is kind of a hack, but I don't expect to receive a value like this if date-range is used directly
  [_]
  nil)

(defmethod date-range :scalar
  [range-value]
  (if range-value
    [range-value range-value]
    [(earliest-date)
     (latest-date)]))

(defmethod date-range :binary
  [[operator date]]
  (case operator
    :<= [(earliest-date) date]
    :< [(earliest-date) (t/minus date (t/days 1))]
    :>= [date (latest-date)]
    :> [(t/plus date (t/days 1)) (latest-date)]))

(defmethod date-range :ternary
  [range-value]
  (-> range-value
      (update-in [1] #(or % (earliest-date)))
      (update-in [2] #(or % (latest-date)))
      rest))

(defmulti parse-date-criterion
  "Accepts a date range in a variety of formats and returns
  a structure like [:between start end].
  
  2015                   => [:between #local-date 2015-01-01 #local-date 2015-12-31]
  2015-03                => [:between #local-date 2015-03-01 #local-date 2015-03-31]
  2015-03-02             => [:between #local-date 2015-03-02 #local-date 2015-03-02]
  #local-date 2015-03-03 => [:between #local-date 2015-03-02 #local-date 2015-03-02]"
  type)

(defmethod parse-date-criterion :default
  [value]
  (if-let [[start end] (date-range value)]
    [:between start end]
    value))

(defmethod parse-date-criterion String
  [value]
  (apply vector :between (dates/parse-range value)))

(defn available-date-range []
  (map #(settings/get %)
       ["earliest-partition-date"
        "latest-partition-date"]))

(defn earliest
  [& dates]
  (let [filtered (filter identity dates)]
    (when (seq filtered)
      (apply min-key to-long filtered))))

(defn latest
  [& dates]
  (let [filtered (filter identity dates)]
    (when (seq filtered)
      (apply max-key to-long filtered))))
