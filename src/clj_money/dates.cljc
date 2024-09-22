(ns clj-money.dates
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.format :as tf])
            #?(:cljs [cljs-time.periodic :as periodic])
            [dgknght.app-lib.core :refer [parse-int]])
  #?(:clj (:import [org.threeten.extra Interval]
                   [java.time ZoneOffset LocalDate Instant]
                   [java.time.temporal ChronoUnit])
     :cljs (:import [goog.date Date DateTime])))

#?(:clj (derive clojure.lang.PersistentVector ::vector)
   :cljs (derive cljs.core.PersistentVector ::vector))
#?(:clj (derive clojure.lang.PersistentArrayMap ::map)
   :cljs (derive cljs.core.PersistentArrayMap ::map))
#?(:clj (derive clojure.lang.PersistentHashMap ::map)
   :cljs (derive cljs.core.PersistentHashMap ::map))
#?(:clj (derive java.lang.String ::string)
   :cljs (derive js/String ::string))

#?(:cljs (extend-protocol IPrintWithWriter
           Date
           (-pr-writer [date writer _]
             (write-all writer "#local-date \"" (tf/unparse (tf/formatters :date) date) "\""))
           DateTime
           (-pr-writer [date writer _]
             (write-all writer "#local-date-time \"" (tf/unparse (tf/formatters :date-time) date) "\""))))

(def local-date?
  #?(:clj t/local-date?
     :cljs (partial instance? Date)))

(defmulti equal?
  (fn [d1 _d2]
    (type d1)))

(defmethod equal? :default
  [d1 d2]
  #?(:clj (t/= d1 d2)
     :cljs (t/equal? d1 d2)))

(defmethod equal? ::vector
  [l1 l2]
  (and (= (count l1)
          (count l2))
       (->> l2
            (interleave l1)
            (partition 2)
            (every? (fn [[d1 d2]]
                      (equal? d1 d2))))))

#?(:clj (defmulti instant* type))

#?(:clj (defmethod instant* LocalDate
          [local-date]
          (.toInstant (.atStartOfDay local-date)
                      ZoneOffset/UTC)))

#?(:clj (defmethod instant* Instant
          [instant]
          instant))

(def year
  #?(:clj (comp #(.getValue %)
                t/year)
     :cljs t/year))

(def month
  #?(:clj (comp #(.getValue %)
                t/month)
     :cljs t/month))

(def instant
  #?(:clj (fn
            ([temporal]
             (instant* temporal))
            ([year month day]
             (instant (t/local-date year month day))))
     :cljs t/date-time))

(def interval
  #?(:clj (fn
            ([[start end]]
             (interval start end))
            ([start end]
             (Interval/of (instant start)
                          (instant end))))
     :cljs t/interval))

(defn- parse-partial
  [value]
  (->> (re-matches #"(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?"
                                          value)
                              rest
                              (map parse-int)))

(defn today []
  #?(:clj (t/local-date)
     :cljs (t/today)))

(defn first-day-of-the-month
  ([] (first-day-of-the-month (today)))
  ([local-date]
   (first-day-of-the-month (t/year local-date)
                           (t/month local-date)))
  ([year month]
   (t/local-date year month 1)))

(defn last-day-of-the-month
  ([] (last-day-of-the-month (today)))
  ([local-date]
   (last-day-of-the-month (t/year local-date)
                          (t/month local-date)))
  ([year month]
   (t/minus (t/plus (first-day-of-the-month year month)
                    (t/months 1))
            (t/days 1))))

(defn last-day-of-the-month?
  [local-date]
  (not= (t/month local-date)
        (t/month (t/plus local-date (t/days 1)))))

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
      [(first-day-of-the-month year month)
       (last-day-of-the-month year month)]

      :else
      [(t/local-date year 1 1)
       (t/local-date year 12 31)])))

(defn parse-interval
  "Takes a string containing a date or partial date and returns a tuple
  containing the inclusive start and exclusive end dates, like [start end].

  2015       => [(t/local-date 2015 1 1) (t/local-date 2016 1 1)]
  2015-03    => [(t/local-date 2015 3 1) (t/local-date 2015 4 1)]
  2015-03-02 => [(t/local-date 2015 3 2) (t/local-date 2015 3 3)]"
  [value]
  (let [[year month day] (parse-partial value)]
    (cond

      day
      (let [start (t/local-date year month day)]
        [start (t/plus start (t/days 1))])

      month
      (let [start (t/local-date year month 1)]
        [start (t/plus start (t/months 1))])

      :else
      (let [start (t/local-date year 1 1)]
        [start (t/plus start (t/years 1))]))))

(defn periodic-seq
  ([start end period]
   (take-while #(not (t/after? % end))
               (periodic-seq start period)))
  ([start period]
   #?(:cljs (periodic/periodic-seq start period)
      :clj (lazy-seq (cons start
                           (periodic-seq (t/plus start period)
                                         period))))))

(defn ranges
  [start interval & {:keys [inclusive]}]
  (let [adj (if inclusive
              #(t/minus % (t/days 1))
              identity)]
    (->> (periodic-seq start interval)
         (partition 2 1)
         (map (fn [[s e]]
                [s (adj e)])))))

(defn desc-periodic-seq
  ([end period-like]
   (lazy-seq (cons end
                   (desc-periodic-seq (t/minus end period-like)
                                      period-like))))
  ([start end period-like]
   {:pre [(t/before? start end)]}

   (take-while #(or (t/after? % start)
                    (equal? % start))
               (desc-periodic-seq end period-like))))

(defn desc-ranges
  ([end period-like]
   (->> (desc-periodic-seq (t/plus end (t/days 1))
                           period-like)
        (partition 2 1)
        (map reverse)))
  ([start end period-like]
   (cond
     (equal? start end)
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
  (->> ds
       (filter identity)
       (sort t/before?)
       first))

(defn latest
  [& ds]
  (->> ds
       (filter identity)
       (sort t/after?)
       first))

(defn within?
  "Return true if the date is in the range, which is inclusive on both ends"
  ([date [start end]]
   (within? date start end))
  ([date start end]
   {:pre [(t/before? start end)]}
   ; false
   ; x
   ;  |---|
   ;
   ;      x
   ; |---|
   ;
   ; true
   ; x
   ; |---|
   ;
   ;  x
   ; |---|
   ;
   ;     x
   ; |---|
   (not (or (t/before? date start)
            (t/before? end date)))))

(defn overlaps?
  "Returns true if the two intervals overlap. Date ranges are inclusive on both ends"
  ([[s1 e1] [s2 e2]]
   (overlaps? s1 e1 s2 e2))
  ([s1 e1 s2 e2]
   {:pre [(t/before? s1 e1)
          (t/before? s2 e2)]}
   ; false
   ; |---|
   ;       |---|
   ;
   ;       |---|
   ; |---|

   ; true
   ; |---|
   ; |---|
   ;
   ;   |---|
   ; |---|
   ;
   ;     |---|
   ; |---|
   ;
   ; |---|
   ;   |---|
   ;
   ; |---|
   ;     |---|

   (not (or (t/before? e1 s2)
            (t/before? e2 s1)))))

(defn day-of-month
  [local-date]
  #?(:clj (.getValue (t/day-of-month local-date))
     :cljs (t/day local-date)))

(defn day-of-week
  [local-date]
  #?(:clj (.getValue (t/day-of-week local-date))
     :cljs (t/day-of-week local-date)))

(defn serialize-local-date
  [local-date]
  #?(:clj (t/format (t/formatter :iso-date) local-date)
     :cljs (tf/unparse-local-date (tf/formatters :date) local-date)))

(defn unserialize-local-date
  [date-str]
  #?(:clj (t/local-date (t/formatter :iso-date) date-str)
     :cljs (tf/parse-local-date (tf/formatters :date) date-str)))

(defn format-local-date
  [local-date]
  #?(:clj (t/format (t/formatter "M/d/yyyy") local-date)
     :cljs (tf/unparse-local-date (tf/formatter "M/d/yyyy") local-date)))

(defn days-between
  [d1 d2]
  #?(:clj (.between ChronoUnit/DAYS d1 d2)
     :cljs (t/in-days (t/interval d1 d2))))

(defmulti ->instant type)

(defmethod ->instant ::string
  [s]
  #?(:clj (t/instant (t/formatter :iso-instant) s)
     :cljs (tf/parse-local-date (tf/formatters :date-time) s)))

(defmacro with-fixed-time
  [time & body]
  #?(:clj `(t/with-clock (t/fixed-clock (->instant ~time)) ~@body)
     :cljs `(t/do-at (->instant ~time) ~@body)))