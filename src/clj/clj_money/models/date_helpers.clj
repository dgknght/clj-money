(ns clj-money.models.date-helpers
  (:require [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date]]
            [clj-money.models.settings :as settings]))

(defmulti date-range
  "Accepts a date criteria value and returns a tuple
  containing the start date in the first position and
  the end date in the second position"
  (fn [range-value]
    (if (sequential? range-value)
      (if (= :between (first range-value))
        :ternary
        :binary)
      :scalar)))

(defn- earliest-date []
  (settings/get (env :db) :earliest-partition-date))

(defn- latest-date []
  (settings/get (env :db) :latest-partition-date))

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

(defmulti parse-date-range
  "Accepts a date range in a variety of formats and returns
  a structure like [:between start end].
  
  2015                   => [:between #local-date 2015-01-01 #local-date 2015-12-31]
  2015-03                => [:between #local-date 2015-03-01 #local-date 2015-03-31]
  2015-03-02             => [:between #local-date 2015-03-02 #local-date 2015-03-02]
  #local-date 2015-03-03 => [:between #local-date 2015-03-02 #local-date 2015-03-02]"
  type)

(defmethod parse-date-range :default
  [value]
  (let [[start end] (date-range value)]
    [:between start end]))

(defmethod parse-date-range String
  [value]
  (let [[year month day] (->> (re-matches #"(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?"
                                          value)
                              rest
                              (map #(when % (Integer. %))))]
    (cond

      day
      [:between
       (t/local-date year month day)
       (t/local-date year month day)]

      month
      [:between
       (to-local-date (t/first-day-of-the-month year month))
       (to-local-date (t/last-day-of-the-month year month))]

      :else
      [:between
       (t/local-date year 1 1)
       (t/local-date year 12 31)])))

(defn available-date-range []
  (map #(settings/get (env :db) %)
       ["earliest-partition-date"
        "latest-partition-date"]))
