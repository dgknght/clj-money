(ns clj-money.dates
  (:require #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.coerce :as tc])
            #?(:clj [clj-time.periodic :refer [periodic-seq]]
               :cljs [cljs-time.periodic :refer [periodic-seq]])
            [dgknght.app-lib.core :refer [parse-int]])
  #?(:import [org.threeten.extra Interval]))

(def ^:private interval
  #?(:clj (fn [start end] (Interval/of start end))
     :cljs t/interval))

(def ^:private instant
  #?(:clj t/instant
     :cljs t/date-time))

(defn- parse-partial
  [value]
  (->> (re-matches #"(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?"
                                          value)
                              rest
                              (map parse-int)))

(def ^:private ->local-date
  #?(:clj t/local-date
     :cljs tc/to-local-date))

(defn first-day-of-the-month
  [year month]
  (t/local-date year month 1))

(defn last-day-of-the-month
  [year month]
  (t/minus (t/plus (first-day-of-the-month year month)
                   (t/months 1))
           (t/days 1)))

(defn parse-range
  "Accepts a date range in a variety of formats and returns
  a tuple containing the start and end dates, like [start end].

  2015                   => [#local-date 2015-01-01 #local-date 2015-12-31]
  2015-03                => [#local-date 2015-03-01 #local-date 2015-03-31]
  2015-03-02             => [#local-date 2015-03-02 #local-date 2015-03-02]
  #local-date 2015-03-03 => [#local-date 2015-03-02 #local-date 2015-03-02]"
  [value]
  (let [[year month day] (parse-partial value)]
    (cond

      day
      [(t/local-date year month day)
       (t/local-date year month day)]

      month
      [(->local-date (first-day-of-the-month year month))
       (->local-date (last-day-of-the-month year month))]

      :else
      [(t/local-date year 1 1)
       (t/local-date year 12 31)])))

(defn parse-interval
  "Takes a string containing a date or partial date and returns a corresponding interval

  2015       => (ten/interval (t/instant 2015 1 1) (t/instant 2016 1 1))
  2015-03    => (ten/interval (t/instant 2015 3 1) (t/instant 2015 4 1))
  2015-03-02 => (ten/interval (t/instant 2015 3 2) (t/instant 2015 3 3))"
  [value]
  (let [[year month day] (parse-partial value)]
    (cond

      day
      (let [start (instant year month day)]
        (interval start
                  (t/plus start
                          (t/days 1))))

      month
      (let [start (instant year month 1)]
        (interval start
                  (t/plus start (t/months 1))))

      :else
      (let [start (instant year 1 1)]
        (interval start
                  (t/plus start (t/years 1)))))))

(defn intervals
  [start interval]
  (->> (periodic-seq start interval)
       (partition 2 1)
       (map (fn [[s e]] (t/interval s e)))))

(defn ranges
  [start interval]
  (->> (periodic-seq start interval)
       (partition 2 1)
       (map (fn [[s e]] [s (t/minus e (t/days 1))]))))

(defn desc-periodic-seq
  ([end period-like]
   (lazy-seq (cons end
                   (desc-periodic-seq (t/minus end period-like)
                                      period-like))))
  ([start end period-like]
   {:pre [(t/before? start end)]}

   (take-while #(or (t/after? % start)
                    (t/equal? % start))
               (desc-periodic-seq end period-like))))

(defn desc-ranges
  ([end period-like]
   (->> (desc-periodic-seq (t/plus end (t/days 1))
                           period-like)
        (partition 2 1)
        (map reverse)))
  ([start end period-like]
   (cond
     (t/equal? start end)
     [[start (t/plus end (t/days 1))]]

     :else
     (take-while #(t/before? start (second %))
                 (desc-ranges end period-like)))))

(defn period
  [interval-type interval-count]
  {:pre [(#{:year :month :week} interval-type)]}

  (case interval-type
    :year (t/years interval-count)
    :month (t/months interval-count)
    :week (t/weeks interval-count)))

(defn earliest
  [& ds]
  (first (sort t/before? ds)))

(defn latest
  [& ds]
  (first (sort t/after? ds)))
