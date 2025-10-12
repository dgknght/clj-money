(ns clj-money.dates
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [postwalk]]
            #?(:cljs [goog.string :refer [format]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.format :as tf])
            #?(:cljs [cljs-time.periodic :as periodic])
            [dgknght.app-lib.core :refer [parse-int]]
            [clj-money.util :as util])
  #?(:clj (:import [org.threeten.extra Interval]
                   [java.time ZoneId ZoneOffset LocalDate Instant]
                   [java.time.temporal ChronoUnit])
     :cljs (:import [goog.date Date DateTime])))

(s/def ::positive-integer (s/and integer? pos?))
(s/def ::period (s/tuple ::positive-integer #{:day :week :month :quarter :year}))

#?(:cljs (extend-type Date
           IEquiv
           (-equiv [this other]
             (and (= (type this)
                     (type other))
                  (t/equal? this other)))

           IComparable
           (-compare [d1 d2]
             (cond
               (t/before? d1 d2) -1
               (t/after? d1 d2)   1
               :else 0))))

(declare serialize-local-date)
(declare serialize-local-date-time)

#?(:cljs (extend-protocol IPrintWithWriter
           Date
           (-pr-writer [date writer _]
             (write-all writer "#clj-money/local-date \"" (serialize-local-date date) "\""))
           DateTime
           (-pr-writer [date writer _]
             (write-all writer "#clj-money/local-date-time \"" (serialize-local-date-time date) "\""))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn local-date
  [x]
  #?(:clj (t/local-date x)
     :cljs (tf/parse-local-date (tf/formatters :date) x)))

(def local-date?
  #?(:clj t/local-date?
     :cljs (partial instance? Date)))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn local-date-time
  [x]
  #?(:clj (t/local-date-time x)
     :cljs (tf/parse (tf/formatters :date-hour-minute-second) x)))

(def local-date-time?
  #?(:clj t/local-date-time?
     :cljs (partial instance? DateTime)))

(def instant?
  #?(:clj t/instant?
     :cljs #(satisfies? t/DateTimeProtocol %)))

(defmulti equal?
  (fn [d1 _d2]
    (cond
      (sequential? d1) :sequence
      (instant? d1) :scalar
      (local-date? d1) :scalar
      (local-date-time? d1) :scalar)))

(defmethod equal? :default
  [& args]
  (pprint {::cannot-compare args
           ::types (map type args)})
  false)

(defmethod equal? :scalar
  [d1 d2]
  #?(:clj (t/= d1 d2)
     :cljs (t/equal? d1 d2)))

(defmethod equal? :sequence
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

#?(:clj (defmethod instant* String
          [instant]
          (t/instant instant)))

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
     :cljs (fn [d]
             (cond; TODO: this should be fleshed out
               (string? d)
               (tf/parse (tf/formatters :date-time-no-ms) d)))))

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

(defn first-day-of-the-year
  ([] (first-day-of-the-year (today)))
  ([date-or-year]
   (let [year (if (local-date? date-or-year)
                (t/year date-or-year)
                date-or-year)]
     (t/local-date year 1 1))))

(defn last-day-of-the-year
  ([] (last-day-of-the-year (today)))
  ([date-or-year]
   (let [year (if (local-date? date-or-year)
                (t/year date-or-year)
                date-or-year)]
     (t/minus (t/plus (t/local-date year 1 1)
                      (t/years 1))
              (t/days 1)))))

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
   {:pre [start period]}
   #?(:cljs (periodic/periodic-seq start period)
      :clj (lazy-seq (cons start
                           (periodic-seq (t/plus start period)
                                         period))))))

(defn ranges
  [start interval & {:keys [inclusive]}]
  {:pre [start interval]}
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
  ([c t] (period [c t]))
  ([[period-count period-type :as period]]
   {:pre [(s/valid? ::period period)]}

   (case period-type
     :year (t/years period-count)
     :month (t/months period-count)
     :week (t/weeks period-count))))

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
   {:pre [(or (= start end)
              (t/before? start end))]}
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

(def outside? (complement within?))

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
  {:pre [local-date]}
  #?(:clj (t/format (t/formatter :iso-date) local-date)
     :cljs (tf/unparse-local-date (tf/formatters :date) local-date)))

(defn unserialize-local-date
  [date-str]
  #?(:clj (t/local-date (t/formatter :iso-date) date-str)
     :cljs (tf/parse-local-date (tf/formatters :date) date-str)))

(defn serialize-local-date-time
  [local-date-time]
  #?(:clj (t/format (t/formatter "yyyy-MM-dd'T'hh:mm:ss") local-date-time)
     :cljs (tf/unparse-local (tf/formatters :date-hour-minute-second) local-date-time)))

(defn unserialize-local-date-time
  [date-str]
  #?(:clj (t/local-date-time (t/formatter :iso-date-time) date-str)
     :cljs (tf/parse-local (tf/formatters :date-hour-minute-second) date-str)))

(defn serialize-instant
  [d]
  #?(:clj (t/format :iso-instant d)
     :cljs (str (tf/unparse (tf/formatters :date-hour-minute-second-ms) d)
                "Z")))

(defn unserialize-instant
  [s]
  #?(:clj (t/instant s)
     :cljs (when s
             (when-let [parsable (re-find #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}" s)]
               (tf/parse (tf/formatters :date-hour-minute-second-ms) parsable)))))

(defn format-local-date
  [local-date]
  #?(:clj (t/format (t/formatter "M/d/yyyy") local-date)
     :cljs (tf/unparse-local-date (tf/formatter "M/d/yyyy") local-date)))

(defn days-between
  [d1 d2]
  #?(:clj (.between ChronoUnit/DAYS d1 d2)
     :cljs (t/in-days (t/interval d1 d2))))

(defmulti ->instant type)

(defmethod ->instant ::util/string
  [s]
  #?(:clj (t/instant (t/formatter :iso-instant) s)
     :cljs (tf/parse-local-date (tf/formatters :date-time) s)))

(defmacro with-fixed-time
  [time & body]
  #?(:clj `(t/with-clock
             (t/mock-clock (t/fixed-clock (->instant ~time))
                           (t/zone-id "UTC"))
             ~@body)
     :cljs `(t/do-at (->instant ~time) ~@body)))

(def ^:private first-and-last
  (juxt first last))

(defn range-boundaries
  [ds]
  {:pre [(every? local-date? ds)]}
  (when (seq ds)
    (first-and-last (sort t/before? ds))))

; TODO: add an option for exclusive end?
(defn push-boundary
  "Given a range, adjust the boundary if necessary to include the specified date"
  [r & ds]
  (->> ds
       (concat r)
       (filter identity)
       range-boundaries))

(defn push-model-boundary
  "Given a model and a key where a date range exists, adjust the boundary
  if necessary to include the specified date"
  [m k & ds]
  {:pre [(every? identity ds)]}
  (apply update-in m [k] push-boundary ds))

#?(:clj (defn zone-id
          [zone-name]
          (ZoneId/of zone-name)))

#?(:clj (defn at-zone
          [instant zone]
          (.atZone instant
                   (if (string? zone)
                     (zone-id zone)
                     zone))))

#?(:clj (defn of-epoch-second
          [second]
          (Instant/ofEpochSecond second)))

(defn serialize-criteria-dates
  [criteria]
  (postwalk (fn [x]
              (if (local-date? x)
                (serialize-local-date x)
                x))
            criteria))

(defn ->local-date
  [inst]
  #?(:clj (t/local-date inst (t/zone-id "UTC"))
     :cljs (t/local-date (t/year inst)
                         (t/month inst)
                         (t/day inst))))

#?(:cljs (defn format-interval
           [i]
           (let [s (t/in-seconds i)]
             (format "%d:%02d"
                     (quot s 60)
                     (mod s 60)))))
