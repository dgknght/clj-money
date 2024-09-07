(ns clj-money.dates-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.dates :as dates]))

(deftest parse-a-date-range
  (is (= [(t/local-date 2015 3 2)
          (t/local-date 2015 3 2)]
         (dates/parse-range "2015-03-02"))
      "A single date is both the start and the end")
  (is (= [(t/local-date 2015 3 1)
          (t/local-date 2015 3 31)]
         (dates/parse-range "2015-03"))
      "A month-year is parsed as the first and last dates of the month")
  (is (= [(t/local-date 2015 1 1)
          (t/local-date 2015 12 31)]
         (dates/parse-range "2015"))
         "A year is parsed as the first and last dates of the year"))

(deftest parse-an-interval
  (is (= (dates/interval (dates/instant 2015 3 1)
                         (dates/instant 2015 4 1))
         (dates/parse-interval "2015-03"))
      "A year-month yields an interval for that month")
  (is (= (dates/interval (dates/instant 2015 1 1)
                         (dates/instant 2016 1 1))
         (dates/parse-interval "2015"))
      "A year yields an interval for that month"))
 
(deftest get-a-sequence-of-intervals
  (is (= [(dates/interval (dates/instant 2015 1 1)
                          (dates/instant 2015 2 1))
          (dates/interval (dates/instant 2015 2 1)
                          (dates/instant 2015 3 1))
          (dates/interval (dates/instant 2015 3 1)
                          (dates/instant 2015 4 1))]
         (take 3 (dates/intervals (t/local-date 2015 1 1)
                                  (t/months 1))))
      "A sequence of intervals of the given size and within the given range is returned"))

(deftest get-a-sequence-of-date-ranges
  (is (= [[(t/local-date 2015 1 1)
           (t/local-date 2015 1 31)]
          [(t/local-date 2015 2 1)
           (t/local-date 2015 2 28)]
          [(t/local-date 2015 3 1)
           (t/local-date 2015 3 31)]]
         (take 3 (dates/ranges (t/local-date 2015 1 1)
                               (t/months 1))))
      "A sequence of date tuples of the given size and within the given range is returned"))

(deftest get-a-descending-sequence-of-periodic-dates
  (testing "bounded result"
    (are [start end interval expected] (= expected
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
    (are [start interval expected] (= expected
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
    (are [start end interval expected] (= expected
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
    (are [start interval expected] (= expected
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
    (is (= jan (dates/earliest jan feb)))
    (is (= jan (dates/earliest feb jan)))))

(deftest get-the-latest-date-from-a-sequence
  (let [jan (t/local-date 2015 1 1)
        feb (t/local-date 2015 2 1)]
    (is (= feb (dates/latest jan feb)))
    (is (= feb (dates/latest feb jan)))))

(deftest get-the-first-day-of-a-month
  (is (= (t/local-date 2015 4 1)
         (dates/first-day-of-the-month 2015 4))))

(deftest get-the-last-day-of-a-month
  (is (= (t/local-date 2015 4 30)
         (dates/last-day-of-the-month 2015 4))))

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
