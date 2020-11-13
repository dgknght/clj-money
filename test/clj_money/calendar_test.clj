(ns clj-money.calendar-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-time.core :as t]
            [clj-money.calendar :as cal]))

(deftest create-a-calendar-map
  (testing "a month that starts on Sunday"
    (let [c (cal/init {:year 2020
                       :month 11
                       :first-day-of-week :sunday})]
      (is c "The return value is not nil")
      (is (= (t/local-date 2020 11 1)
             (:date (ffirst (:weeks c))))
          "The first day is the first of the month")))
  (testing "a month that starts on Tuesday"
    (let [c (cal/init {:year 2020
                       :month 9
                       :first-day-of-week :sunday})]
      (is c "The return value is not nil")
      (is (= (t/local-date 2020 8 30)
             (:date (ffirst (:weeks c))))
          "The first day is the penultimate day of the previous month")))
  (testing "a month that starts on Saturday"
    (let [c (cal/init {:year 2020
                       :month 8
                       :first-day-of-week :sunday})]
      (is c "The return value is not nil")
      (is (= (t/local-date 2020 7 26)
             (:date (ffirst (:weeks c))))
          "The first day is 6 days from the end of the previous month"))))

