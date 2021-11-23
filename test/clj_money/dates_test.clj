(ns clj-money.dates-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clj-time.core :as t]
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
  (is (= (t/interval (t/date-time 2015 3 1)
                     (t/date-time 2015 4 1))
         (dates/parse-interval "2015-03"))
      "A year-month yields an interval for that month")
  (is (= (t/interval (t/date-time 2015 1 1)
                     (t/date-time 2016 1 1))
         (dates/parse-interval "2015"))
      "A year yields an interval for that month"))

(deftest get-a-sequence-of-intervals
  (is (= [(t/interval (t/date-time 2015 1 1)
                      (t/date-time 2015 2 1))
          (t/interval (t/date-time 2015 2 1)
                      (t/date-time 2015 3 1))
          (t/interval (t/date-time 2015 3 1)
                      (t/date-time 2015 4 1))]
         (take 3 (dates/intervals (t/date-time 2015 1 1)
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
