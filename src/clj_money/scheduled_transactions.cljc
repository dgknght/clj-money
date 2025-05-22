(ns clj-money.scheduled-transactions
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.predicates :as tp])
            [clj-money.dates :as dates]))

(defn- seq-start
  [{:scheduled-transaction/keys [start-date last-occurrence]
    [_ period-type :as period] :scheduled-transaction/period}]
  (or (when last-occurrence
        (t/plus last-occurrence (if (= :week period-type)
                                  (t/days 1)
                                  (dates/period period))))
      start-date))

(defmulti ^:private date-seq (comp second :scheduled-transaction/period))

; spec - {:month m :day d}
(defmethod date-seq :year
  [{[period-count] :scheduled-transaction/period
    {:keys [month day]} :scheduled-transaction/date-spec
    :as sched-trx}]
  (let [first-date (->> (dates/periodic-seq (seq-start sched-trx)
                                            (t/days 1))
                        (take 366)
                        (filter #(and (= month (dates/month %))
                                      (= day (dates/day-of-month %))))
                        first)]
    (dates/periodic-seq first-date (t/years period-count))))

; spec - {:day 1}, {:day :last}
(defmethod date-seq :month
  [{[period-count] :scheduled-transaction/period
    {:keys [day]} :scheduled-transaction/date-spec
    :as sched-trx}]
  (let [first-date (->> (dates/periodic-seq (seq-start sched-trx)
                                            (t/days 1))
                        (take 31)
                        (filter #(if (or (= :last day)
                                         (< (dates/day-of-month (dates/last-day-of-the-month %))
                                            day))
                                   (dates/last-day-of-the-month? %)
                                   (= day (dates/day-of-month %))))
                        first)]
    (cond->> (dates/periodic-seq first-date (t/months period-count))
      (= :last day) (map dates/last-day-of-the-month))))

; spec - {:day [:monday :friday]}
(def ^:private weekday-predicates
  #?(:clj {:sunday t/sunday?
           :monday t/monday?
           :tuesday t/tuesday?
           :wednesday t/wednesday?
           :thursday t/thursday?
           :friday t/friday?
           :saturday t/saturday?}
     :cljs {:sunday tp/sunday?
            :monday tp/monday?
            :tuesday tp/tuesday?
            :wednesday tp/wednesday?
            :thursday tp/thursday?
            :friday tp/friday?
            :saturday tp/saturday?}))

(defmethod date-seq :week
  [{[period-count] :scheduled-transaction/period
    {:keys [days]} :scheduled-transaction/date-spec
    :as sched-trx}]
  (let [pred (apply some-fn (map weekday-predicates days))]
    (->> (dates/periodic-seq (seq-start sched-trx) (t/days 1))
         (take 7)
         (filter pred) ; get one start date for each specified day of the week
         (map #(dates/periodic-seq % (t/weeks period-count))) ; each start date generates a sequence
         (apply interleave)))) ; interleaving the sequences creates on sequence in chronological order

(defn next-transaction-dates
  "Returns dates in the transaction date sequence for the next x number of days (default 7)"
  ([sched-trx]
   (next-transaction-dates sched-trx 7))
  ([{:scheduled-transaction/keys [last-occurrence end-date] :as sched-trx} days-out]
   (let [lower-bound (or last-occurrence
                         (t/local-date 1900 1 1))
         upper-bound (->> [end-date
                           (t/plus (dates/today) (t/days days-out))]
                          (filter identity)
                          (sort t/before?)
                          first)]
     (->> (date-seq sched-trx)
          (take-while #(not (t/after? % upper-bound)))
          (filter #(t/after? % lower-bound))))))

(defn next-transaction-date
  "Returns the next time the scheduled transaction would be created"
  [sched-trx]
  (first (next-transaction-dates sched-trx 365)))

(defn- ->transaction-item
  [{:scheduled-transaction-item/keys [account
                                      action
                                      quantity
                                      memo]}]
  #:transaction-item{:account account
                     :action action
                     :quantity quantity
                     :memo memo})

(defn ->transaction
  ([sched-trx]
   (partial ->transaction sched-trx))
  ([{:as sched-trx
     :scheduled-transaction/keys [description
                                  memo
                                  entity
                                  items]}
    transaction-date]
   #:transaction{:transaction-date transaction-date
                 :description description
                 :memo memo
                 :entity entity
                 :items (map ->transaction-item items)
                 :scheduled-transaction sched-trx}))

(defn realize
  "Creates new transactions based on the scheduled transaction,
  if the date of the new transactions would be within one week
  of the current date"
  [sched-trx]
  (when-let [created (->> (next-transaction-dates sched-trx)
                          (map (->transaction sched-trx))
                          seq)]
    (cons (assoc sched-trx
                 :scheduled-transaction/last-occurrence
                 (->> created
                      (map :transaction/transaction-date)
                      (sort t/after?)
                      first))
          created)))

(def ^:private max-date (dates/local-date "9999-12-31"))

(defn started?
  [{:scheduled-transaction/keys [start-date]}]
  (t/before? start-date (dates/today)))

(defn ended?
  [{:scheduled-transaction/keys [end-date]}]
  (t/before? (or end-date max-date)
             (dates/today)))

(def not-ended? (complement ended?))

(defn enabled?
  [{:scheduled-transaction/keys [enabled]}]
  enabled)

(defn occurring-soon?
  [{:scheduled-transaction/keys [next-occurrence]}]
  (t/before? next-occurrence (t/plus (dates/today) (t/days 7))))

(def pending?
  (every-pred started?
              not-ended?
              enabled?
              occurring-soon?))
