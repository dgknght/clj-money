(ns clj-money.scheduled-transactions
  (:require #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.coerce :refer [to-local-date]]
               :cljs [cljs-time.coerce :refer [to-local-date]])
            #?(:clj [clj-time.periodic :refer [periodic-seq]]
               :cljs [cljs-time.periodic :refer [periodic-seq]])
            #?(:clj [clj-time.predicates :as tp :refer [last-day-of-the-month?]]
               :cljs [cljs-time.predicates :as tp])
            [clojure.set :refer [rename-keys]]))

#?(:cljs (defn- last-day-of-the-month?
           [d]
           (t/equal? (t/last-day-of-the-month d) d)))

(defmulti ^:private since-last :interval-type)

(defmethod since-last :year
  [{:keys [last-occurrence interval-count]}]
  (when last-occurrence
    (t/plus last-occurrence (t/years interval-count))))

(defmethod since-last :month
  [{:keys [last-occurrence interval-count]}]
  (when last-occurrence
    (t/plus last-occurrence (t/months interval-count))))

(defmethod since-last :week
  [{:keys [last-occurrence]}]
  (when last-occurrence
    (t/plus last-occurrence (t/days 1))))

(defn- seq-start
  [{:keys [start-date] :as sched-tran}]
  (or (since-last sched-tran)
      start-date))

(defmulti ^:private date-seq :interval-type)

; spec - {:month m :day d}
(defmethod date-seq :year
  [{:keys [interval-count] {:keys [month day]} :date-spec :as sched-tran}]
  (let [first-date (->> (periodic-seq (seq-start sched-tran)
                                      (t/days 1))
                        (take 366)
                        (filter #(and (= month (t/month %))
                                      (= day (t/day %))))
                        first)]
    (periodic-seq first-date (t/years interval-count))))

; spec - {:day 1}, {:day :last}
(defmethod date-seq :month
  [{:keys [interval-count] {:keys [day]} :date-spec :as sched-tran}]
  (let [first-date (->> (periodic-seq (seq-start sched-tran)
                                      (t/days 1))
                        (take 31)
                        (filter #(if (or (= :last day)
                                         (< (t/day (t/last-day-of-the-month %))
                                            day))
                                   (last-day-of-the-month? %)
                                   (= day (t/day %))))
                        first)]
    (cond->> (periodic-seq first-date (t/months interval-count))
      (= :last day) (map t/last-day-of-the-month))))

; spec - {:day [:monday :friday]}
(def ^:private weekday-predicates
  {:sunday tp/sunday?
   :monday tp/monday?
   :tuesday tp/tuesday?
   :wednesday tp/wednesday?
   :thursday tp/thursday?
   :friday tp/friday?
   :saturday tp/saturday?})

(defmethod date-seq :week
  [{:keys [interval-count] {:keys [days]} :date-spec :as sched-tran}]
  (let [pred (apply some-fn (map weekday-predicates days))]
    (->> (periodic-seq (seq-start sched-tran) (t/days 1))
         (take 7)
         (filter pred) ; get one start date for each specified day of the week
         (map #(periodic-seq % (t/weeks interval-count))) ; each start date generates a sequence
         (apply interleave)))) ; interleaving the sequences creates on sequence in chronological order

(defn next-transaction-dates
  "Returns dates in the transaction date sequence for the next x number of days (default 7)"
  ([sched-tran]
   (next-transaction-dates sched-tran 7))
  ([{:keys [last-occurrence end-date] :as sched-tran} days-out]
   (let [lower-bound (or last-occurrence
                         (to-local-date (t/epoch)))
         upper-bound (->> [end-date
                           (t/plus (t/today) (t/days days-out))]
                          (filter identity)
                          (sort t/before?)
                          first)]
     (->> (date-seq sched-tran)
          (take-while #(not (t/after? % upper-bound)))
          (filter #(t/after? % lower-bound))))))

(defn next-transaction-date
  "Returns the next time the scheduled transaction would be created"
  [sched-tran]
  (first (next-transaction-dates sched-tran 365)))

(defn ->transaction
  [sched-tran transaction-date]
  (-> sched-tran
      (rename-keys {:id :scheduled-transaction-id})
      (select-keys [:description
                    :memo
                    :entity-id
                    :items
                    :scheduled-transaction-id])
      (assoc :transaction-date transaction-date)))

(defn pending?
  [{:keys [enabled start-date end-date next-occurrence]}]
  (and enabled
       (t/after? (t/today) start-date)
       (or (nil? end-date)
           (t/before? (t/today) end-date))
       (t/before? next-occurrence
                  (t/plus (t/today) (t/days 7)))))
