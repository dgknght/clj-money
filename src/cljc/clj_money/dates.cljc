(ns clj-money.dates
  (:require #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.coerce :as tc]
               :cljs [cljs-time.coerce :as tc])
            #?(:clj [clj-time.periodic :refer [periodic-seq]]
               :cljs [cljs-time.periodic :refer [periodic-seq]])
            [dgknght.app-lib.core :refer [parse-int]]))

(defn- parse-partial
  [value]
  (->> (re-matches #"(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?"
                                          value)
                              rest
                              (map parse-int)))

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
      [(tc/to-local-date (t/first-day-of-the-month year month))
       (tc/to-local-date (t/last-day-of-the-month year month))]

      :else
      [(t/local-date year 1 1)
       (t/local-date year 12 31)])))

(defn parse-interval
  "Takes a string containing a date or partial date and returns a corresponding interval

  2015       => (t/interval (t/date-time 2015 1 1) (t/date-time 2016 1 1))
  2015-03    => (t/interval (t/date-time 2015 3 1) (t/date-time 2015 4 1))
  2015-03-02 => (t/interval (t/date-time 2015 3 2) (t/date-time 2015 3 3))"
  [value]
  (let [[year month day] (parse-partial value)]
    (cond

      day
      (let [start (t/date-time year month day)]
        (t/interval start
                    (t/plus start
                            (t/days 1))))

      month
      (let [start (t/date-time year month 1)]
        (t/interval start
                    (t/plus start (t/months 1))))

      :else
      (let [start (t/date-time year 1 1)]
        (t/interval start
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
