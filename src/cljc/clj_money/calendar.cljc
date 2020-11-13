(ns clj-money.calendar
  (:require #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.coerce :refer [to-local-date]]
               :cljs [cljs-time.coerce :refer [to-local-date]])
            #?(:clj [clj-time.periodic :refer [periodic-seq]]
               :cljs [cljs-time.periodic :refer [periodic-seq]])))

(def month-names
  ["January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December"])

(def day-data
  [{:abbreviation "Mon"
    :name "Monday"}
   {:abbreviation "Tue"
    :name "Tuesday"}
   {:abbreviation "Wed"
    :name "Wednesday"}
   {:abbreviation "Thu"
    :name "Thursday"}
   {:abbreviation "Fri"
    :name "Friday"}
   {:abbreviation "Sat"
    :name "Saturday"}
   {:abbreviation "Sun"
    :name "Sunday"}])

(def ^:private days-of-the-week
  (->> [:monday
        :tuesday
        :wednesday
        :thursday
        :friday
        :saturday
        :sunday]
       (map-indexed #(vector %2 (+ 1 %1)))
       (into {})))

; Library day-of-week values
; 1 - Monday
; 2 - Tuesday
; 3 - Wednesday
; 4 - Thursday
; 5 - Friday
; 6 - Saturday
; 7 - Sunday
(def ^:private offsets
  {:monday 0
   :sunday 1})

(defn- start-offset
  [first-dom {:keys [first-day-of-week]}]
  {:pre [(contains? offsets first-day-of-week)]}
  (if (= (t/day-of-week first-dom)
         (days-of-the-week first-day-of-week))
    0
    (+ (- (t/day-of-week first-dom)
          1)
       (get-in offsets [first-day-of-week]))))

(defn- day-info
  [{:keys [first-day-of-week]}]
  (->> day-data
       cycle
       (drop (- 7 (get-in offsets [first-day-of-week])))
       (take 7)))

(defn- date->map
  [date month opts]
  {:date date
   :today? (t/equal? date (t/today))
   :selected? (and (:selected opts)
                   (t/equal? date (:selected opts)))
   :in-month? (= month (t/month date))})

(defn- ensure-year-month
  [{:keys [selected year month] :as opts}]
  (cond
    (and year month)
    opts

    selected
    (assoc opts
           :year (t/year selected)
           :month (t/month selected))

    :else
    (let [today (t/today)]
      (assoc opts
             :year (t/year today)
             :month (t/month today)))))

(def ^:private defaults
  {:first-day-of-week :sunday})

(defn- prepare-opts
  [opts]
  (-> (merge defaults opts)
      ensure-year-month))

(defn init
  ([]
   (let [today (t/today)]
     (init {:year (t/year today)
            :month (t/month today)})))
  ([options]
   (let [{:keys [year month] :as opts} (prepare-opts options)
         first-dom (t/first-day-of-the-month year month)
         offset (start-offset first-dom opts)
         start (t/minus first-dom (t/days offset))
         days (periodic-seq start (t/days 1))]
     (assoc opts
            :title (str (nth month-names (- month 1)) " " year)
            :week-days (day-info opts)
            :weeks (->> days
                        (map (comp #(date->map % month opts)
                                   to-local-date))
                        (partition 7)
                        (take-while #(some :in-month? %))
                        (map vec)
                        (into []))))))

(defn next-month
  [{:keys [year month options]}]
  (let [[y m] (if (= month 12)
                [(inc year) 1]
                [year (inc month)])]
    (init (assoc options
                 :year y
                 :month m))))

(defn prev-month
  [{:keys [year month options]}]
  (let [[y m] (if (= month 1)
                [(dec year) 12]
                [year (dec month)])]
    (init (assoc options
                 :year y
                 :month m))))

(defn select
  [calendar date]
  (init (assoc calendar :selected date)))
