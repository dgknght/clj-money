(ns clj-money.dates-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest is testing are]])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :as dates]))

(deftest compare-two-local-dates-for-equality
  (testing "simple dates"
    (is (dates/equal? (t/local-date 2000 1 1)
                      (t/local-date 2000 1 1))
        "Two instances of the same calendar date are equal")
    (is (not (dates/equal? (t/local-date 2000 1 1)
                           (t/local-date 2000 1 2)))
        "Two instances of the different calendar date are not equal"))
  (testing "sequences of dates"
    (is (dates/equal? [(t/local-date 2000 1 1)
                       (t/local-date 2000 1 2)]
                      [(t/local-date 2000 1 1)
                       (t/local-date 2000 1 2)])
        "Two sequences containing the same dates are equal")
    (is (not (dates/equal? [(t/local-date 2000 1 1)
                            (t/local-date 2000 1 2)]
                           [(t/local-date 2000 1 2)
                            (t/local-date 2000 1 3)]))
        "Two sequences containing the different dates are not equal")
    (is (not (dates/equal? [(t/local-date 2000 1 1)
                            (t/local-date 2000 1 2)]
                           [(t/local-date 2000 1 1)
                            (t/local-date 2000 1 2)
                            (t/local-date 2000 1 3)]))
        "Two sequences containing a different number of dates are not equal")))

(deftest get-the-year-from-a-local-date
  (is (= 2000 (dates/year (t/local-date 2000 1 1)))))

(deftest get-the-month-from-a-local-date
  (is (= 3 (dates/month (t/local-date 2000 3 1)))))

(deftest parse-a-date-range
  (let [[start end] (dates/parse-range "2015-03-02")]
    (is (dates/equal? (t/local-date 2015 3 2) start)
        "The start is the specified date")
    (is (dates/equal? (t/local-date 2015 3 2) end)
        "The end is the specified date"))
  (let [[start end] (dates/parse-range "2015-03")]
    (is (dates/equal? (t/local-date 2015 3 1) start)
        "The start is the first day of a specified month")
    (is (dates/equal? (t/local-date 2015 3 31) end)
        "The end is the last day of a specified month"))
  (let [[start end] (dates/parse-range "2015")]
    (is (dates/equal? (t/local-date 2015 1 1) start)
        "The start is the first day of a specified year")
    (is (dates/equal? (t/local-date 2015 12 31) end)
        "The end is the last day of a specified year")))

(deftest parse-an-interval
  (is (dates/equal? [(t/local-date 2015 3 1)
                     (t/local-date 2015 4 1)] 
                    (dates/parse-interval "2015-03"))
      "A year-month yields an interval for that month")
  (is (dates/equal? [(t/local-date 2015 1 1)
                     (t/local-date 2016 1 1)]
                    (dates/parse-interval "2015"))
      "A year yields an interval for that year"))

(deftest get-a-sequence-of-date-ranges
  (is (dates/equal?
        [[(t/local-date 2015 1 1)
          (t/local-date 2015 2 1)]
         [(t/local-date 2015 2 1)
          (t/local-date 2015 3 1)]
         [(t/local-date 2015 3 1)
          (t/local-date 2015 4 1)]]
        (take 3 (dates/ranges (t/local-date 2015 1 1)
                              (t/months 1))))
      "A sequence of date tuples (exclusive upper bound) of the given size and within the given range is returned")
  (is (dates/equal?
        [[(t/local-date 2015 1 1)
          (t/local-date 2015 1 31)]
         [(t/local-date 2015 2 1)
          (t/local-date 2015 2 28)]
         [(t/local-date 2015 3 1)
          (t/local-date 2015 3 31)]]
        (take 3 (dates/ranges (t/local-date 2015 1 1)
                              (t/months 1)
                              :inclusive true)))
      "A sequence of date tuples (exclusive upper bound) of the given size and within the given range is returned"))

(deftest get-a-descending-sequence-of-periodic-dates
  (testing "bounded result"
    (are [start end interval expected] (dates/equal?
                                         expected
                                         (dates/desc-periodic-seq start end interval))
         (t/local-date 2015 9 1)
         (t/local-date 2015 12 1)
         (t/months 1)
         [(t/local-date 2015 12 1)
          (t/local-date 2015 11 1)
          (t/local-date 2015 10 1)
          (t/local-date 2015 9 1)]

         (t/local-date 2015 9 1)
         (t/local-date 2015 12 1)
         (t/months 2)
         [(t/local-date 2015 12 1)
          (t/local-date 2015 10 1)]))
  (testing "unbounded result"
    (are [start interval expected] (dates/equal?
                                     expected
                                     (take 3 (dates/desc-periodic-seq start interval)))
         (t/local-date 2015 12 1)
         (t/months 1)
         [(t/local-date 2015 12 1)
          (t/local-date 2015 11 1)
          (t/local-date 2015 10 1)]

         (t/local-date 2015 12 1)
         (t/months 2)
         [(t/local-date 2015 12 1)
          (t/local-date 2015 10 1)
          (t/local-date 2015 8 1)])))

(deftest get-a-descending-sequence-of-date-ranges
  (testing "bounded result"
    (are [start end interval expected] (dates/equal?
                                         expected
                                         (dates/desc-ranges start end interval))
         (t/local-date 2015 9 1)
         (t/local-date 2015 12 1)
         (t/months 1)
         [[(t/local-date 2015 11 2) (t/local-date 2015 12 2)] ; must include the end date, top number is exclusive
          [(t/local-date 2015 10 2) (t/local-date 2015 11 2)]
          [(t/local-date 2015  9 2) (t/local-date 2015 10 2)]
          [(t/local-date 2015  8 2) (t/local-date 2015  9 2)]] ; must include the start date, bottom number is inclusive

         (t/local-date 2015 9 1)
         (t/local-date 2015 12 1)
         (t/months 2)
         [[(t/local-date 2015 10 2) (t/local-date 2015 12 2)]
          [(t/local-date 2015  8 2) (t/local-date 2015 10 2)]]

         (t/local-date 2015 12 1)
         (t/local-date 2015 12 1)
         (t/months 1)
         [[(t/local-date 2015 12 1) (t/local-date 2015 12 2)]]))
  (testing "unbounded result"
    (are [start interval expected] (dates/equal?
                                     expected
                                     (take 3 (dates/desc-ranges start interval)))
         (t/local-date 2015 12 1)
         (t/months 1)
         [[(t/local-date 2015 11 2) (t/local-date 2015 12 2)] ; must include the end date, top number is exclusive
          [(t/local-date 2015 10 2) (t/local-date 2015 11 2)]
          [(t/local-date 2015  9 2) (t/local-date 2015 10 2)]]

         (t/local-date 2015 12 1)
         (t/months 2)
         [[(t/local-date 2015 10 2) (t/local-date 2015 12 2)]
          [(t/local-date 2015  8 2) (t/local-date 2015 10 2)]
          [(t/local-date 2015  6 2) (t/local-date 2015  8 2)]])))

(deftest get-the-earliest-date-from-a-sequence
  (let [jan (t/local-date 2015 1 1)
        feb (t/local-date 2015 2 1)]
    (is (dates/equal? jan (dates/earliest jan feb)))
    (is (dates/equal? jan (dates/earliest feb jan)))
    (is (dates/equal? jan (dates/earliest nil jan)))
    (is (dates/equal? jan (dates/earliest jan nil)))))

(deftest get-the-latest-date-from-a-sequence
  (let [jan (t/local-date 2015 1 1)
        feb (t/local-date 2015 2 1)]
    (is (dates/equal? feb (dates/latest jan feb)))
    (is (dates/equal? feb (dates/latest feb jan)))))

(deftest get-the-first-day-of-the-year
  (is (= (t/local-date 2020 1 1)
         (dates/first-day-of-the-year 2020))
      "It returns the first day of the specified year when given a number")
  (is (= (t/local-date 2020 1 1)
         (dates/first-day-of-the-year (t/local-date 2020 3 2)))
      "It returns the first day of the same year when given a date"))

(deftest get-the-last-day-of-the-year
  (is (= (t/local-date 2020 12 31)
         (dates/last-day-of-the-year 2020))
      "It returns the last day of the specified year when given a number")
  (is (= (t/local-date 2020 12 31)
         (dates/last-day-of-the-year (t/local-date 2020 3 2)))
      "It returns the last day of the same year when given a date"))

(deftest get-the-first-day-of-a-month
  (is (dates/equal? (t/local-date 2015 4 1)
                    (dates/first-day-of-the-month 2015 4))))

(deftest get-the-last-day-of-a-month
  (is (dates/equal? (t/local-date 2015 4 30)
                    (dates/last-day-of-the-month 2015 4))))

(deftest evaluate-last-day-of-the-month
  (is (dates/last-day-of-the-month? (t/local-date 2000 3 31)))
  (is (not (dates/last-day-of-the-month? (t/local-date 2000 3 30)))))

(deftest see-if-a-date-is-in-a-range
  (are [date range expected] (= expected (dates/within? date range))

       ; before the start
       (t/local-date 1999 12 31)
       [(t/local-date 2000 1 1)
        (t/local-date 2000 1 3)]
       false 

       ; between start and end
       (t/local-date 2000 1 2)
       [(t/local-date 2000 1 1)
        (t/local-date 2000 1 3)]
       true

       ; on the start
       (t/local-date 2000 1 1)
       [(t/local-date 2000 1 1)
        (t/local-date 2000 1 3)]
       true

       ; on the end
       (t/local-date 2000 1 1)
       [(t/local-date 2000 1 1)
        (t/local-date 2000 1 3)]
       true

       ; after the end
       (t/local-date 2000 1 4)
       [(t/local-date 2000 1 1)
        (t/local-date 2000 1 3)]
       false))

(deftest see-if-date-ranges-overlap
  (are [r1 r2 expected] (= expected (dates/overlaps? r1 r2))

       ; r1 is entirely before r2
       [(t/local-date 2000 1 1)
        (t/local-date 2000 1 31)]
       [(t/local-date 2000 2 1)
        (t/local-date 2000 2 29)]
       false

       ; r1 starts before r2 and ends after r2 starts
       [(t/local-date 2000 1 1)
        (t/local-date 2000 2 2)]
       [(t/local-date 2000 2 1)
        (t/local-date 2000 2 29)]
       true

       ; r1 is fully enclosed by r2
       [(t/local-date 2000 2 2)
        (t/local-date 2000 2 28)]
       [(t/local-date 2000 2 1)
        (t/local-date 2000 2 29)]
       true

       ; r1 starts within r2 and ends after
       [(t/local-date 2000 2 2)
        (t/local-date 2000 3 1)]
       [(t/local-date 2000 2 1)
        (t/local-date 2000 2 29)]
       true

       ; r1 starts within r2 and ends after
       [(t/local-date 2000 2 2)
        (t/local-date 2000 3 1)]
       [(t/local-date 2000 2 1)
        (t/local-date 2000 2 29)]
       true

       ; r1 starts after r2 ends
       [(t/local-date 2000 3 1)
        (t/local-date 2000 3 2)]
       [(t/local-date 2000 2 1)
        (t/local-date 2000 2 29)]
       false))

(deftest format-a-local-date
  (is (= "1/1/2000"
         (dates/format-local-date (t/local-date 2000 1 1)))))

(deftest serialize-a-local-date
  (is (= "2000-01-01"
         (dates/serialize-local-date (t/local-date 2000 1 1)))))

(deftest unserialize-a-local-date
  (is (dates/equal? (t/local-date 2000 1 1)
                    (dates/unserialize-local-date "2000-01-01"))))

(deftest serialize-a-local-date-time
  (is (= "2000-01-01T12:34:56.000"
         (dates/serialize-local-date-time (t/local-date-time 2000 1 1 12 34 56)))))

(deftest unserialize-a-local-date-time
  (is (dates/equal? (t/local-date-time 2000 1 1 11 22 33)
                    (dates/unserialize-local-date-time "2000-01-01T11:22:33"))))

(deftest get-the-number-of-days-in-a-period
  (is (= 3 (dates/days-between (t/local-date 2000 1 1)
                               (t/local-date 2000 1 4)))))

(deftest get-range-boundaries
  (let [[start end] (dates/range-boundaries [(t/local-date 2000 1 15)
                                             (t/local-date 2000 1 31)
                                             (t/local-date 2000 1 1)])]
    (is (dates/equal? (t/local-date 2000 1 1)
                      start)
        "The range start date is in the first position of the return value.")
    (is (dates/equal? (t/local-date 2000 1 31)
                      end)
        "The range end date is in the second position of the return value.")))
